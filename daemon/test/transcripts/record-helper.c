/* HS-5 lossless helper dialogue recorder.  Build only for the differential gate. */
#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <unistd.h>

extern char **environ;
static FILE *log;
static void record(const char *direction, const char *bytes, size_t size) {
  fprintf(log, "%s %zu ", direction, size);
  for (size_t i = 0; i < size; i++) fprintf(log, "%02x", (unsigned char)bytes[i]);
  fputc('\n', log);
  fflush(log);
}
static int pipe_cloexec(int fds[2]) {
  if (pipe(fds) < 0) return -1;
  if (fcntl(fds[0], F_SETFD, FD_CLOEXEC) < 0 ||
      fcntl(fds[1], F_SETFD, FD_CLOEXEC) < 0) return -1;
  return 0;
}
static int forward(int source, int destination, const char *direction) {
  char bytes[32768];
  ssize_t size = read(source, bytes, sizeof bytes);
  if (size > 0) {
    record(direction, bytes, (size_t)size);
    const char *at = bytes;
    while (size > 0) {
      ssize_t written = write(destination, at, (size_t)size);
      if (written < 0) { if (errno == EINTR) continue; break; }
      at += written;
      size -= written;
    }
    return 0;
  } else if (size == 0) {
    if (source == STDIN_FILENO) close(destination);
    return 1;
  }
  return errno == EINTR ? 0 : -1;
}
int main(int argc, char **argv) {
  const char *path = getenv("GOONIX_HELPER_TRANSCRIPT");
  const char *real = getenv("GOONIX_REAL_HELPER");
  if (!path || !real || argc < 2) return 64;
  log = fopen(path, "w");
  if (!log) return 65;
  fprintf(log, "argv");
  for (int i = 1; i < argc; i++) fprintf(log, " %s", argv[i]);
  fprintf(log, "\n");
  const char *options = getenv("_NIX_OPTIONS");
  fprintf(log, "cwd /\n_NIX_OPTIONS %s\n", options ? options : "");
  int input[2], output[2], builder[2];
  if (pipe_cloexec(input) || pipe_cloexec(output) || pipe_cloexec(builder)) return 66;
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attributes;
  int result = posix_spawn_file_actions_init(&actions);
  if (result || (result = posix_spawn_file_actions_adddup2(&actions, input[0], 0)) ||
      (result = posix_spawn_file_actions_adddup2(&actions, output[1], 1)) ||
      (result = posix_spawn_file_actions_adddup2(&actions, output[1], 2)) ||
      (result = posix_spawn_file_actions_adddup2(&actions, builder[1], 4)) ||
      (result = posix_spawn_file_actions_addclose(&actions, input[0])) ||
      (result = posix_spawn_file_actions_addclose(&actions, output[1])) ||
      (result = posix_spawn_file_actions_addclose(&actions, builder[1])) ||
      (result = posix_spawn_file_actions_addchdir_np(&actions, "/"))) return 67;
  if ((result = posix_spawnattr_init(&attributes)) ||
      (result = posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETPGROUP)) ||
      (result = posix_spawnattr_setpgroup(&attributes, 0))) return 68;
  pid_t pid;
  result = posix_spawn(&pid, real, &actions, &attributes, argv, environ);
  posix_spawn_file_actions_destroy(&actions);
  posix_spawnattr_destroy(&attributes);
  if (result) return 69;
  close(input[0]); close(output[1]); close(builder[1]);
  struct pollfd fds[3] = {{0, POLLIN, 0}, {output[0], POLLIN, 0}, {builder[0], POLLIN, 0}};
  int open_fds = 3;
  while (open_fds) {
    if (poll(fds, 3, -1) < 0) { if (errno == EINTR) continue; return 70; }
    for (int i = 0; i < 3; i++) if (fds[i].fd >= 0 && fds[i].revents) {
      int finished = forward(fds[i].fd, i == 0 ? input[1] : i == 1 ? 1 : 4,
              i == 0 ? "stdin" : i == 1 ? "stdout" : "fd4");
      if (finished != 0) {
        close(fds[i].fd);
        fds[i].fd = -1;
        open_fds--;
        if (finished < 0) return 70;
      }
    }
  }
  close(input[1]); close(output[0]); close(builder[0]);
  int status;
  if (waitpid(pid, &status, 0) < 0) return 71;
  fprintf(log, "raw_status %d\n", status);
  fclose(log);
  if (WIFEXITED(status)) return WEXITSTATUS(status);
  if (WIFSIGNALED(status)) { raise(WTERMSIG(status)); return 128 + WTERMSIG(status); }
  return 1;
}
