#include <caml/version.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#ifndef _MSC_VER
#include <unistd.h>
#endif

#if OCAML_VERSION < 41200
#define Val_none Val_int(0)
#define Tag_some 0

static value caml_alloc_some(value v)
{
	CAMLparam1(v);
	value some = caml_alloc_small(1, Tag_some);
	Field(some, 0) = v;
	CAMLreturn(some);
}
#endif

// Detect platform
#if defined(_WIN32)
#define OCAML_ALCOTEST_WINDOWS
#elif defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__))
#if defined(_POSIX_VERSION)
#define OCAML_ALCOTEST_POSIX
#endif
#endif

// Windows support
#if defined(OCAML_ALCOTEST_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <windows.h>

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);

	CONSOLE_SCREEN_BUFFER_INFO csbi;
	int success = GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &csbi);
	if (success)
	{
		pair = caml_alloc_tuple(2);
		Store_field(pair, 0, Val_int((int)(csbi.dwSize.Y)));
		Store_field(pair, 1, Val_int((int)(csbi.dwSize.X)));
		result = caml_alloc_some(pair);
	}
	else
	{
		result = Val_none;
	}

	CAMLreturn(result);
}

// POSIX support
#elif defined(OCAML_ALCOTEST_POSIX)
#include <sys/ioctl.h>

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal2(result, pair);
	struct winsize ws;
	int z = ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws);
	if (z == 0)
	{
		pair = caml_alloc_tuple(2);
		Store_field(pair, 0, Val_int(ws.ws_row));
		Store_field(pair, 1, Val_int(ws.ws_col));
		result = caml_alloc_some(pair);
	}
	else
	{
		result = Val_none;
	}

	CAMLreturn(result);
}

// Unsupported platform
#else

CAMLprim value ocaml_alcotest_get_terminal_dimensions(value unit)
{
	CAMLparam1(unit);
	CAMLlocal1(result);

	result = Val_none;
	CAMLreturn(result);
}

#endif

/* duplicated from caml/sys.h and io.c */
CAMLextern value caml_channel_descriptor(value vchannel);
#define NO_ARG Val_int(0)
CAMLextern void caml_sys_error (value);
/* End of code duplication */

static int alcotest_saved_stdout;
static int alcotest_saved_stderr;

CAMLprim value alcotest_before_test (value voutput, value vstdout, value vstderr) {
  int output_fd, stdout_fd, stderr_fd, fd, ret;
  stdout_fd = Int_val(caml_channel_descriptor(vstdout));
  stderr_fd = Int_val(caml_channel_descriptor(vstderr));
  output_fd = Int_val(caml_channel_descriptor(voutput));
  fd = dup(stdout_fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  alcotest_saved_stdout = fd;
  fd = dup(stderr_fd);
  if(fd == -1) caml_sys_error(NO_ARG);
  alcotest_saved_stderr = fd;
  ret = dup2(output_fd, stdout_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(output_fd, stderr_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}

CAMLprim value alcotest_after_test (value vstdout, value vstderr) {
  int stdout_fd, stderr_fd, ret;
  stdout_fd = Int_val(caml_channel_descriptor(vstdout));
  stderr_fd = Int_val(caml_channel_descriptor(vstderr));
  ret = dup2(alcotest_saved_stdout, stdout_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = dup2(alcotest_saved_stderr, stderr_fd);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(alcotest_saved_stdout);
  if(ret == -1) caml_sys_error(NO_ARG);
  ret = close(alcotest_saved_stderr);
  if(ret == -1) caml_sys_error(NO_ARG);
  return Val_unit;
}
