/* sim_readline.h: declarations for libreadline wrapper

   Written by Philipp Hachtmann


   19-SEP-08    PH     Initial version


*/

#ifndef READLINE_H
#define READLINE_H
#ifdef HAVE_READLINE

extern char * sim_readline_readline(char *cpre, int32 size, const char * prompt);

extern int sim_readline_start();
extern int sim_readline_stop();

#endif
#endif
