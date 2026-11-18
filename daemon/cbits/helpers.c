/* goonix: descriptor wiring for exec-only helper processes. */
#define _GNU_SOURCE
#include <spawn.h>
#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

extern char **environ;

int goonix_spawn_sandbox(const char *program, const char *plan,
                        int log_fd, int *pid_out)
{
  int pid = fork();
  if (pid < 0) return -1;
  if (pid == 0) {
    /* Do not run Haskell between fork and exec.  Only this tiny C shim runs. */
    if (dup2(log_fd, STDOUT_FILENO) < 0 || dup2(log_fd, STDERR_FILENO) < 0)
      _exit(127);
    if (log_fd > STDERR_FILENO) close(log_fd);
    execl(program, program, plan, (char *)NULL);
    _exit(127);
  }
  *pid_out = pid;
  return 0;
}

int goonix_spawn_helper(const char *program, const char *argument,
                        int input_fd, int output_fd, int reply_fd,
                        const char *options, int *pid_out)
{
  posix_spawn_file_actions_t actions;
  int result = posix_spawn_file_actions_init(&actions);
  if (result != 0) { errno = result; return -1; }
  if ((result = posix_spawn_file_actions_adddup2(&actions, input_fd, STDIN_FILENO)) != 0 ||
      (result = posix_spawn_file_actions_adddup2(&actions, output_fd, STDOUT_FILENO)) != 0 ||
      (result = posix_spawn_file_actions_adddup2(&actions, output_fd, STDERR_FILENO)) != 0 ||
      (reply_fd >= 0 && (result = posix_spawn_file_actions_adddup2(&actions, reply_fd, 4)) != 0) ||
      (result = posix_spawn_file_actions_addclose(&actions, input_fd)) != 0 ||
      (result = posix_spawn_file_actions_addclose(&actions, output_fd)) != 0 ||
      (reply_fd >= 0 && (result = posix_spawn_file_actions_addclose(&actions, reply_fd)) != 0)) {
    posix_spawn_file_actions_destroy(&actions);
    errno = result;
    return -1;
  }
  char *const argv[] = {(char *)program, (char *)"substitute", (char *)argument, NULL};
  size_t count = 0;
  while (environ[count]) count++;
  char **child_environment = calloc(count + 2, sizeof(char *));
  if (!child_environment) { errno = ENOMEM; return -1; }
  size_t index = 0;
  for (size_t i = 0; i < count; i++) {
    if (strncmp(environ[i], "_NIX_OPTIONS=", 13) != 0) child_environment[index++] = environ[i];
  }
  size_t option_length = strlen(options);
  char *option = malloc(option_length + 14);
  if (!option) { free(child_environment); errno = ENOMEM; return -1; }
  memcpy(option, "_NIX_OPTIONS=", 13);
  memcpy(option + 13, options, option_length + 1);
  child_environment[index++] = option;
  child_environment[index] = NULL;
  pid_t pid;
  result = posix_spawn(&pid, program, &actions, NULL, argv, child_environment);
  free(option);
  free(child_environment);
  posix_spawn_file_actions_destroy(&actions);
  if (result != 0) { errno = result; return -1; }
  *pid_out = (int)pid;
  return 0;
}
