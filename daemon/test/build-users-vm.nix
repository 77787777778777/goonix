# HS-6 root/build-user gate. The daemon runs as root only inside the guest.
{ pkgs ? import <nixpkgs> { }
, goonixDaemon
, guixClient ? pkgs.guix
, cxxDaemon ? pkgs.guix
}:

pkgs.testers.nixosTest {
  name = "goonix-hs6-build-users";
  nodes.machine = { pkgs, ... }:
    let staticShell = pkgs.pkgsStatic.busybox;
        # One static store item: its applet links resolve inside that item, so
        # every declared input closure also contains the interpreter target.
        tools = staticShell; in {
    virtualisation.memorySize = 1024;
    users.groups.guixbuild = {
      gid = 31000;
      members = [ "guixbuilder01" "guixbuilder02" ];
    };
    users.groups.guixkvm.gid = 31001;
    users.users.guixbuilder01 = {
      isSystemUser = true;
      uid = 31002;
      group = "guixbuild";
      extraGroups = [ "guixkvm" ];
    };
    users.users.guixbuilder02 = {
      isSystemUser = true;
      uid = 31003;
      group = "guixbuild";
    };
    services.guix.enable = false;
    systemd.services.goonix-daemon = {
      description = "Goonix Haskell daemon build-user VM gate";
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = "${goonixDaemon}/bin/guix-daemon --build-users-group=guixbuild --chroot-directory=/tmp/hs6-markers --substitute-urls=file:///tmp/hs6-substitutes --listen=/tmp/guix-daemon.sock";
        Environment = [
          "GUIX=${guixClient}/bin/guix"
          "GUIX_STATE_DIRECTORY=/var/guix"
          "GUIX_SANDBOX=${goonixDaemon}/bin/guix-sandbox"
          "GUIX_BINARY_SUBSTITUTE_URL=file:///tmp/hs6-substitutes"
          "GUIX_TEST_SUBSTITUTE_URLS=file:///tmp/hs6-substitutes"
          "GUIX_ALLOW_UNAUTHENTICATED_SUBSTITUTES=yes"
          "GUIX_LOG_DIRECTORY=/tmp/hs6-logs"
          "GUILE_LOAD_PATH=${guixClient}/share/guile/site/3.0"
          "GUILE_LOAD_COMPILED_PATH=${guixClient}/lib/guile/3.0/site-ccache"
        ];
        Restart = "no";
      };
    };
    environment.systemPackages = [ guixClient pkgs.guix pkgs.coreutils pkgs.guile pkgs.gawk pkgs.procps pkgs.sqlite ];
    environment.variables.GUILE_LOAD_PATH = "${guixClient}/share/guile/site/3.0";
    environment.variables.GUILE_LOAD_COMPILED_PATH = "${guixClient}/lib/guile/3.0/site-ccache";
    environment.variables.GUIX_BINARY_SUBSTITUTE_URL = "file:///tmp/hs6-substitutes";
    environment.variables.GUIX_TEST_SUBSTITUTE_URLS = "file:///tmp/hs6-substitutes";
    environment.variables.GUIX_STATE_DIRECTORY = "/var/guix";
    environment.etc."hs6-build-users.scm".text = ''
      (use-modules (guix store) (guix derivations) (ice-9 threads)
                   (ice-9 textual-ports))
      (define store (open-connection "/tmp/guix-daemon.sock"))
      (define markers "/tmp/hs6-markers")
      (define (marker name suffix) (string-append markers "/" name suffix))
      (set-build-options store #:use-substitutes? #f #:offload? #f #:max-build-jobs 2)
      (define shell (add-to-store store "sh" #t "sha256" "${tools}/bin/sh"))
      (define id (add-to-store store "id" #t "sha256" "${tools}/bin/id"))
      (define sleep (add-to-store store "sleep" #t "sha256" "${tools}/bin/sleep"))
      (define (make-drv name)
        (let ((builder
               (add-text-to-store store (string-append name ".sh")
                 (string-append
                   "printf '%s %s %s\\n' \"$(" id " id -u)\" \"$(" id " id -g)\" \"$(" id " id -G)\" > \"$out\"\n"
                   "printf '%s\\n' \"$out\" > /tmp/hs6-markers/" name ".started\n"
                   "while ! test -e /tmp/hs6-markers/" name ".release; do " sleep " 1; done\n"))))
          (derivation store name shell (list "sh" "-e" builder)
                      #:sources (list shell id sleep builder))))
      (define one (make-drv "hs6-one"))
      (define two (make-drv "hs6-two"))
      (call-with-output-file "/tmp/hs6-client.ready" (lambda (p) (display "ready\n" p)))
      (build-derivations store (list one two))
      (for-each (lambda (name drv)
                  (let ((output (derivation->output-path drv)))
                    (unless (valid-path? store output) (error "missing output" output))
                    (call-with-output-file (string-append "/tmp/hs6-" name ".output")
                      (lambda (port) (display output port)))
                    (call-with-output-file (string-append "/tmp/hs6-" name ".identity")
                      (lambda (port) (display (call-with-input-file output get-string-all) port)))))
                (list "one" "two") (list one two))
      (call-with-output-file "/tmp/hs6-client.done" (lambda (p) (display "done\n" p)))
      (close-connection store)
    '';
    environment.etc."hs6-cleanup.scm".text = ''
      (use-modules (guix store) (guix derivations) (ice-9 textual-ports))
      (define store (open-connection "/tmp/guix-daemon.sock"))
      (set-build-options store #:use-substitutes? #f #:offload? #f #:max-build-jobs 2)
      (define shell (add-to-store store "sh" #t "sha256" "${tools}/bin/sh"))
      (define id (add-to-store store "id" #t "sha256" "${tools}/bin/id"))
      (define sleep (add-to-store store "sleep" #t "sha256" "${tools}/bin/sleep"))
      (define (make-drv name leave-process?)
        (let ((builder
               (add-text-to-store store (string-append name ".sh")
                 (string-append
                   "printf '%s %s %s\\n' \"$(" id " id -u)\" \"$(" id " id -g)\" \"$(" id " id -G)\" > \"$out\"\n"
                   "printf '%s\\n' \"$out\" > /tmp/hs6-markers/" name ".started\n"
                   (if leave-process?
                     (string-append
                       sleep " 300 </dev/null >/dev/null 2>&1 &\n"
                       "printf '%s\\n' $! > /tmp/hs6-markers/hs6-leftover.pid\n")
                     "")
                   "while ! test -e /tmp/hs6-markers/" name ".release; do " sleep " 1; done\n"))))
          (derivation store name shell (list "sh" "-e" builder)
                      #:sources (list shell id sleep builder))))
      (define first (make-drv "hs6-leave-process" #t))
      (define second (make-drv "hs6-reuse" #f))
      (build-derivations store (list first))
      (call-with-output-file "/tmp/hs6-first.identity"
        (lambda (port) (display (call-with-input-file (derivation->output-path first) get-string-all) port)))
      (call-with-output-file "/tmp/hs6-first.done" (lambda (port) (display "done\n" port)))
      (build-derivations store (list second))
      (call-with-output-file "/tmp/hs6-second.identity"
        (lambda (port) (display (call-with-input-file (derivation->output-path second) get-string-all) port)))
      (call-with-output-file "/tmp/hs6-cleanup.done" (lambda (p) (display "done\n" p)))
      (close-connection store)
    '';
    environment.etc."hs6-reverse-exclusion.scm".text = ''
      (use-modules (guix store) (guix derivations) (ice-9 threads)
                   (ice-9 textual-ports))
      (define store (open-connection "/tmp/guix-daemon.sock"))
      (define shell (add-to-store store "sh" #t "sha256" "${tools}/bin/sh"))
      (define id (add-to-store store "id" #t "sha256" "${tools}/bin/id"))
      (define sleep (add-to-store store "sleep" #t "sha256" "${tools}/bin/sleep"))
      (define (make-drv name)
        (let ((builder (add-text-to-store store (string-append name ".sh")
                 (string-append "printf '%s\\n' \"$(" id " id -u)\" >$out\n"
                                "while ! test -e /tmp/hs6-markers/reverse.release; do " sleep " 1; done\n"))))
          (derivation store name shell (list "sh" "-e" builder)
                      #:sources (list shell id sleep builder))))
      (define one (make-drv "hs6-reverse-one"))
      (define two (make-drv "hs6-reverse-two"))
      (set-build-options store #:use-substitutes? #f #:offload? #f #:max-build-jobs 2)
      (build-derivations store (list one two))
      (call-with-output-file "/tmp/hs6-reverse.identities"
        (lambda (port)
          (for-each (lambda (drv)
                      (display (call-with-input-file (derivation->output-path drv) get-string-all) port))
                    (list one two))))
      (close-connection store)
    '';
    environment.etc."hs6-substitute-collect.scm".text = ''
      (use-modules (guix store) (guix derivations) (guix tests)
                   (ice-9 textual-ports) (srfi srfi-11))
      (define (assert condition message)
        (unless condition (error message)))
      (define store (open-connection "/tmp/guix-daemon.sock"))
      (define drv (derivation store "hs6-substituted" "/does-not-exist" '()))
      (define output (derivation->output-path drv))
      (define root "/tmp/hs6-markers/substitute-root")
      (set-build-options store #:use-substitutes? #t #:fallback? #f
                         #:substitute-urls (%test-substitute-urls))
      (with-derivation-substitute drv "root-substituted-output\n"
        (ensure-path store output))
      (assert (valid-path? store output) "substitute did not register output")
      (assert (string=? "root-substituted-output\n"
                        (call-with-input-file output get-string-all))
              "substitute content differs")
      (symlink output root)
      (add-indirect-root store root)
      (let-values (((paths freed) (collect-garbage store)))
        (assert (file-exists? output) "collection deleted rooted substitute")
        (assert (valid-path? store output) "collection invalidated rooted substitute"))
      (assert (= 0 (stat:uid (stat output))) "substitute is not root-owned")
      (call-with-output-file "/tmp/hs6-substitute.path"
        (lambda (port) (display output port)))
      (call-with-output-file "/tmp/hs6-substitute-collect.done"
        (lambda (port) (display "done\n" port)))
      (close-connection store)
    '';
    environment.etc."hs6-exclusion.scm".text = ''
      (use-modules (guix store) (guix derivations))
      (define store (open-connection "/tmp/cxx-daemon.sock"))
      (set-build-options store #:use-substitutes? #f #:offload? #f #:max-build-jobs 2)
      (define shell (add-to-store store "sh" #t "sha256" "${tools}/bin/sh"))
      (define sleep (add-to-store store "sleep" #t "sha256" "${tools}/bin/sleep"))
      (define id (add-to-store store "id" #t "sha256" "${tools}/bin/id"))
      (define builder
        (add-text-to-store store "hs6-exclusion.sh"
          (string-append
            "printf '%s\\n' \"$(" id " id -u)\" >/tmp/cxx-markers/exclusion.uid\n"
            "while ! test -e /tmp/cxx-markers/exclusion.release; do " sleep " 1; done\n")))
      (build-derivations store (list (derivation store "hs6-exclusion" shell (list "sh" "-e" builder) #:sources (list shell id sleep builder))))
    '';
  };
  testScript = ''
    machine.wait_for_unit("goonix-daemon.service")
    machine.succeed("test -S /tmp/guix-daemon.sock")
    machine.succeed("test \"$(id -u guixbuilder01)\" = 31002")
    machine.succeed("test \"$(id -u guixbuilder02)\" = 31003")
    machine.succeed("mkdir -p /tmp/hs6-markers /tmp/cxx-markers /tmp/hs6-substitutes && chmod 1777 /tmp/hs6-markers /tmp/cxx-markers")
    try:
        machine.succeed("guix repl -- /etc/hs6-substitute-collect.scm >/tmp/hs6-substitute-collect.log 2>&1")
    except Exception:
        machine.succeed("cat /tmp/hs6-substitute-collect.log; journalctl -u goonix-daemon.service --no-pager; find /tmp/hs6-substitutes -maxdepth 1 -type f -print; false")
        raise
    machine.wait_for_file("/tmp/hs6-substitute-collect.done")
    machine.succeed("test -f /var/guix/db/db.sqlite")
    machine.succeed("sqlite3 /var/guix/db/db.sqlite \"pragma busy_timeout=10000; select name from sqlite_master where type='table'\" | grep -q '^ValidPaths$'")
    machine.succeed("test \"$(stat -c %u:%g \"$(cat /tmp/hs6-substitute.path)\")\" = 0:0")
    machine.succeed("output=$(cat /tmp/hs6-substitute.path); stat -c '%u:%g %a %A %n' \"$output\"; case $(stat -c %a \"$output\") in *[2367]*) exit 1;; esac")
    machine.succeed("systemctl show goonix-daemon.service -p Environment -p ExecStart")
    machine.succeed("guix repl -- /etc/hs6-build-users.scm >/tmp/hs6-client.log 2>&1 &")
    machine.wait_for_file("/tmp/hs6-client.ready")
    for name in ("one", "two"):
        with machine.nested(f"waiting for builder {name} or client failure"):
            try:
                machine.wait_for_file(f"/tmp/hs6-markers/hs6-{name}.started", timeout=45)
            except Exception:
                machine.succeed("cat /tmp/hs6-client.log; journalctl -u goonix-daemon.service --no-pager; test ! -d /tmp/hs6-logs || find /tmp/hs6-logs -type f -exec sh -c 'echo --- $1; zcat $1' sh {} \\;; false")
                raise
    machine.succeed("ps -eo uid,gid,stat,args")
    machine.succeed("ps -eo uid=,args= | grep -q '^ *31002 .*hs6-one'")
    machine.succeed("ps -eo uid=,args= | grep -q '^ *31003 .*hs6-two'")
    # C++ must skip both UIDs already leased by the Haskell daemon.
    machine.succeed("GUIX_STATE_DIRECTORY=/tmp/cxx-var GUIX_LOG_DIRECTORY=/tmp/cxx-var/log/guix GUIX_DAEMON_SOCKET=/tmp/cxx-daemon.sock ${cxxDaemon}/bin/guix-daemon --build-users-group=guixbuild --disable-chroot --no-substitutes --listen=/tmp/cxx-daemon.sock >/tmp/cxx-daemon.log 2>&1 &")
    machine.succeed("for i in $(seq 1 100); do test -S /tmp/cxx-daemon.sock && break; done; test -S /tmp/cxx-daemon.sock")
    machine.succeed("GUIX_DAEMON_SOCKET=/tmp/cxx-daemon.sock guix repl -- /etc/hs6-exclusion.scm >/tmp/cxx-client.log 2>&1 &")
    machine.wait_for_file("/tmp/cxx-daemon.log")
    machine.wait_until_succeeds("grep -q 'all build users are currently in use' /tmp/cxx-client.log")
    # Restart the control after that exhaustion check and keep its builder
    # alive while Haskell attempts to lease the same UID.
    machine.succeed("pkill -f 'guix-daemon.*cxx-daemon.sock' || true")
    machine.succeed("rm -f /tmp/cxx-daemon.sock /tmp/cxx-client.log /tmp/cxx-daemon.log")
    machine.succeed("GUIX_STATE_DIRECTORY=/tmp/cxx-var GUIX_LOG_DIRECTORY=/tmp/cxx-var/log/guix GUIX_DAEMON_SOCKET=/tmp/cxx-daemon.sock ${cxxDaemon}/bin/guix-daemon --build-users-group=guixbuild --disable-chroot --no-substitutes --listen=/tmp/cxx-daemon.sock >/tmp/cxx-daemon.log 2>&1 &")
    machine.succeed("for i in $(seq 1 100); do test -S /tmp/cxx-daemon.sock && break; sleep .1; done; test -S /tmp/cxx-daemon.sock")
    machine.succeed("GUIX_DAEMON_SOCKET=/tmp/cxx-daemon.sock guix repl -- /etc/hs6-exclusion.scm >/tmp/cxx-client.log 2>&1 &")
    machine.wait_for_file("/tmp/cxx-markers/exclusion.uid")
    machine.succeed("test \"$(cat /tmp/cxx-markers/exclusion.uid)\" = 31002 -o \"$(cat /tmp/cxx-markers/exclusion.uid)\" = 31003")
    # While C++ holds one user, Haskell must use the other and reject a second
    # concurrent lease rather than taking the C++ user's UID.
    machine.succeed("guix repl -- /etc/hs6-reverse-exclusion.scm >/tmp/hs6-reverse.log 2>&1 &")
    machine.succeed("for i in $(seq 1 100); do ps -eo uid=,args= | grep -q 'hs6-reverse-one\\|hs6-reverse-two' && break; sleep .1; done")
    machine.succeed("ps -eo uid=,args= | grep 'hs6-reverse-one\\|hs6-reverse-two' >/tmp/hs6-reverse.ps")
    machine.succeed("test \"$(awk '$1 == 31002 || $1 == 31003 { count[$1]++ } END { for (uid in count) print uid }' /tmp/hs6-reverse.ps)\" != \"$(cat /tmp/cxx-markers/exclusion.uid)\"")
    machine.wait_until_succeeds("grep -q 'all build users are currently in use' /tmp/hs6-reverse.log")
    machine.succeed("touch /tmp/hs6-markers/reverse.release")
    machine.wait_for_file("/tmp/hs6-reverse.identities")
    machine.succeed("test \"$(cat /tmp/hs6-reverse.identities | wc -l)\" = 1")
    machine.succeed("pkill -f 'guix-daemon.*cxx-daemon.sock' || true")
    machine.succeed("touch /tmp/hs6-one.release /tmp/hs6-two.release")
    machine.wait_for_file("/tmp/hs6-client.done")
    machine.succeed("cat /tmp/hs6-client.log")
    machine.succeed("cat /tmp/hs6-*.identity")
    machine.succeed("grep -q '^31002 31000 .*31001' /tmp/hs6-*.identity")
    machine.succeed("grep -q '^31003 31000 ' /tmp/hs6-*.identity")
    machine.succeed("ps -eo uid=,stat= | awk '$1 == 31002 || $1 == 31003 { if ($2 !~ /^Z/) found = 1 } END { exit found ? 1 : 0 }'")
    machine.succeed("for record in /tmp/hs6-one.output /tmp/hs6-two.output; do output=$(cat \"$record\"); stat -c '%u:%g %a %A %n' \"$output\"; test \"$(stat -c %u:%g \"$output\")\" = 0:0; case $(stat -c %a \"$output\") in *[2367]*) exit 1;; esac; done")
    machine.succeed("test -e /var/guix/userpool/31002 -a -e /var/guix/userpool/31003")
    machine.succeed("guix repl -- /etc/hs6-cleanup.scm >/tmp/hs6-cleanup.log 2>&1 &")
    machine.wait_for_file("/tmp/hs6-leftover.pid")
    machine.wait_for_file("/tmp/hs6-markers/hs6-leave-process.started")
    machine.succeed("uid=$(ps -o uid= -p $(cat /tmp/hs6-leftover.pid)); test \"$uid\" = 31002 -o \"$uid\" = 31003; printf '%s\n' \"$uid\" >/tmp/hs6-reuse.uid")
    machine.succeed("touch /tmp/hs6-leave-process.release")
    machine.wait_for_file("/tmp/hs6-first.done")
    machine.succeed("! kill -0 $(cat /tmp/hs6-leftover.pid) 2>/dev/null")
    machine.wait_for_file("/tmp/hs6-markers/hs6-reuse.started")
    machine.succeed("touch /tmp/hs6-reuse.release")
    machine.wait_for_file("/tmp/hs6-cleanup.done")
    machine.succeed("cat /tmp/hs6-first.identity /tmp/hs6-second.identity")
    machine.succeed("test \"$(cut -d' ' -f1 /tmp/hs6-first.identity)\" = \"$(cat /tmp/hs6-reuse.uid)\"")
    machine.succeed("test \"$(cut -d' ' -f1 /tmp/hs6-second.identity)\" = \"$(cat /tmp/hs6-reuse.uid)\"")
    machine.succeed("ps -eo uid=,stat= | awk '$1 == 31002 || $1 == 31003 { if ($2 !~ /^Z/) found = 1 } END { exit found ? 1 : 0 }'")
  '';
}
