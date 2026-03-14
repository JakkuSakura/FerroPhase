#include <stdint.h>
#include <stddef.h>
#include <stdlib.h>
#include <inttypes.h>
#include <stdio.h>
#include <dlfcn.h>
#include <sys/types.h>
#include <sys/xattr.h>
#include <wchar.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>

#include <sys/stat.h>
#include <dirent.h>

// Some Apple SDKs do not ship a C11 <uchar.h>. We only need `char32_t` for a
// minimal ASCII-only `mbrtoc32` shim.
#ifndef FP_CHAR32_T_DEFINED
#define FP_CHAR32_T_DEFINED
typedef uint32_t char32_t;
#endif

// macOS headers implement some *_unlocked APIs as macros. We need real symbols
// with those names for glibc-built binaries, so undefine the macros first.
#ifdef getc_unlocked
#undef getc_unlocked
#endif
#ifdef putc_unlocked
#undef putc_unlocked
#endif
#ifdef stdin
#undef stdin
#endif
#ifdef stdout
#undef stdout
#endif
#ifdef stderr
#undef stderr
#endif

// Minimal glibc symbol shims for Mach-O execution.
//
// These are intentionally tiny: enough to let simple Linux-built binaries
// load on macOS while the compatibility layer is still under development.

extern FILE *__stderrp;
extern FILE *stdin;
extern FILE *stdout;
extern FILE *stderr;
extern FILE *__stdinp;
extern FILE *__stdoutp;

static void fp_init_stdio_globals_if_needed(void) {
  if (stdin == NULL) {
    stdin = __stdinp;
  }
  if (stdout == NULL) {
    stdout = __stdoutp;
  }
  if (stderr == NULL) {
    stderr = __stderrp;
  }
}

static int fp_shim_trace_enabled(void) {
  const char *v = getenv("FP_SHIM_TRACE");
  return v && v[0] != '\0';
}

__attribute__((visibility("default")))
int fprintf(FILE *stream, const char *format, ...) {
  fp_init_stdio_globals_if_needed();
  if (stream == NULL) {
    stream = __stderrp;
  }

  va_list args;
  va_start(args, format);
  int rc = vfprintf(stream, format, args);
  va_end(args);
  return rc;
}

__attribute__((visibility("default")))
int __printf_chk(int flag, const char *format, ...) {
  (void)flag;
  fp_init_stdio_globals_if_needed();

  va_list args;
  va_start(args, format);
  int rc = vfprintf(stdout ? stdout : __stdoutp, format, args);
  va_end(args);
  return rc;
}

__attribute__((visibility("default")))
int __fprintf_chk(FILE *stream, int flag, const char *format, ...) {
  (void)flag;
  fp_init_stdio_globals_if_needed();
  if (stream == NULL) {
    stream = __stderrp;
  }

  va_list args;
  va_start(args, format);
  int rc = vfprintf(stream, format, args);
  va_end(args);
  return rc;
}

__attribute__((visibility("default")))
int __vfprintf_chk(FILE *stream, int flag, const char *format, va_list ap) {
  (void)flag;
  fp_init_stdio_globals_if_needed();
  if (stream == NULL) {
    stream = __stderrp;
  }
  return vfprintf(stream, format, ap);
}

__attribute__((visibility("default")))
int __sprintf_chk(char *s, int flag, size_t slen, const char *format, ...) {
  (void)flag;
  (void)slen;

  va_list args;
  va_start(args, format);
  int rc = vsprintf(s, format, args);
  va_end(args);
  return rc;
}

__attribute__((visibility("default")))
int __snprintf_chk(char *s,
                   size_t maxlen,
                   int flag,
                   size_t slen,
                   const char *format,
                   ...) {
  (void)flag;
  (void)slen;

  va_list args;
  va_start(args, format);
  int rc = vsnprintf(s, maxlen, format, args);
  va_end(args);
  return rc;
}

__attribute__((visibility("default")))
void __assert_fail(const char *assertion,
                   const char *file,
                   unsigned int line,
                   const char *function) {
  // Report and abort. If this triggers in real Linux binaries, it likely
  // indicates we still have a semantic mismatch in lifted code.
  const uintptr_t a = (uintptr_t)assertion;
  const uintptr_t f = (uintptr_t)file;
  const uintptr_t fn = (uintptr_t)function;
  const char *assertion_text = a >= 0x100000000ULL ? assertion : "(badptr)";
  const char *file_text = f >= 0x100000000ULL ? file : "(badptr)";
  const char *function_text = fn >= 0x100000000ULL ? function : "(badptr)";

  dprintf(2,
          "[fp] glibc assert failed: %s:%u: %s: %s\n",
          file_text,
          line,
          function_text,
          assertion_text);
  fflush(NULL);

  exit(127);
}

__attribute__((visibility("default")))
int statx(int dirfd,
          const char *pathname,
          int flags,
          unsigned int mask,
          void *statxbuf) {
  (void)dirfd;
  (void)pathname;
  (void)flags;
  (void)mask;
  (void)statxbuf;

  // Best-effort compatibility: many Linux programs treat `statx` as an
  // optional optimization and fall back to `stat` when it is unavailable.
  errno = ENOSYS;
  return -1;
}

__attribute__((visibility("default")))
void *mempcpy(void *dest, const void *src, size_t n) {
  (void)memcpy(dest, src, n);
  return (void *)((uint8_t *)dest + n);
}

__attribute__((visibility("default")))
void *__mempcpy_chk(void *dest, const void *src, size_t n, size_t destlen) {
  (void)destlen;
  return mempcpy(dest, src, n);
}

__attribute__((visibility("default")))
void *__memcpy_chk(void *dest, const void *src, size_t n, size_t destlen) {
  (void)destlen;
  return memcpy(dest, src, n);
}

__attribute__((visibility("default")))
void *__memmove_chk(void *dest, const void *src, size_t n, size_t destlen) {
  (void)destlen;
  return memmove(dest, src, n);
}

__attribute__((visibility("default")))
void *__memset_chk(void *dest, int c, size_t n, size_t destlen) {
  (void)destlen;
  return memset(dest, c, n);
}

__attribute__((visibility("default")))
char *__strcpy_chk(char *dest, const char *src, size_t destlen) {
  (void)destlen;
  return strcpy(dest, src);
}

__attribute__((visibility("default")))
char *__strncpy_chk(char *dest, const char *src, size_t n, size_t destlen) {
  (void)destlen;
  return strncpy(dest, src, n);
}

__attribute__((visibility("default")))
ssize_t __readlink_chk(const char *path, char *buf, size_t len, size_t buflen) {
  (void)buflen;
  return readlink(path, buf, len);
}

__attribute__((visibility("default")))
void *rawmemchr(const void *s, int c) {
  const unsigned char *p = (const unsigned char *)s;
  unsigned char needle = (unsigned char)c;
  for (;;) {
    if (*p == needle) {
      return (void *)p;
    }
    p++;
  }
}

// glibc exposes a locale-aware character classification table via this symbol.
// Many Linux-built binaries use it indirectly through ctype macros.
//
// We implement a minimal ASCII-only table that covers the common cases.
enum {
  FP_ISBLANK = 0x0001,
  FP_ISCNTRL = 0x0002,
  FP_ISPUNCT = 0x0004,
  FP_ISALNUM = 0x0008,
  FP_ISUPPER = 0x0100,
  FP_ISLOWER = 0x0200,
  FP_ISALPHA = 0x0400,
  FP_ISDIGIT = 0x0800,
  FP_ISXDIGIT = 0x1000,
  FP_ISSPACE = 0x2000,
  FP_ISPRINT = 0x4000,
  FP_ISGRAPH = 0x8000,
};

static unsigned short fp_ctype_b[384];
static const unsigned short *fp_ctype_b_ptr = fp_ctype_b + 128;
static int fp_ctype_b_initialized = 0;

static void fp_init_ctype_b(void) {
  if (fp_ctype_b_initialized) {
    return;
  }
  fp_ctype_b_initialized = 1;

  for (int c = 0; c < 256; c++) {
    unsigned short flags = 0;
    if (c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\v' ||
        c == '\f') {
      flags |= FP_ISSPACE;
      if (c == ' ' || c == '\t') {
        flags |= FP_ISBLANK;
      }
    }
    if (c < 0x20 || c == 0x7f) {
      flags |= FP_ISCNTRL;
    }
    if (c >= 0x20 && c <= 0x7e) {
      flags |= FP_ISPRINT;
      if (c != ' ') {
        flags |= FP_ISGRAPH;
      }
    }

    int is_upper = (c >= 'A' && c <= 'Z');
    int is_lower = (c >= 'a' && c <= 'z');
    int is_digit = (c >= '0' && c <= '9');
    int is_alpha = is_upper || is_lower;
    int is_alnum = is_alpha || is_digit;
    int is_xdigit = is_digit || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');

    if (is_upper) {
      flags |= FP_ISUPPER;
    }
    if (is_lower) {
      flags |= FP_ISLOWER;
    }
    if (is_alpha) {
      flags |= FP_ISALPHA;
    }
    if (is_digit) {
      flags |= FP_ISDIGIT;
    }
    if (is_xdigit) {
      flags |= FP_ISXDIGIT;
    }
    if (is_alnum) {
      flags |= FP_ISALNUM;
    }

    if ((flags & FP_ISGRAPH) && !is_alnum) {
      flags |= FP_ISPUNCT;
    }

    fp_ctype_b[c + 128] = flags;
  }
}

__attribute__((visibility("default")))
const unsigned short **__ctype_b_loc(void) {
  fp_init_ctype_b();
  return &fp_ctype_b_ptr;
}

__attribute__((visibility("default")))
size_t __ctype_get_mb_cur_max(void) {
  // Minimal ASCII-only locale.
  return 1;
}

extern int *__error(void);

__attribute__((visibility("default")))
int *__errno_location(void) {
  return __error();
}

__attribute__((visibility("default")))
uintmax_t __isoc23_strtoumax(const char *nptr, char **endptr, int base) {
  return strtoumax(nptr, endptr, base);
}

extern char **environ;

__attribute__((visibility("default")))
int __libc_start_main(int (*main_fn)(int, char **, char **),
                      int argc,
                      char **argv,
                      void (*init)(void),
                      void (*fini)(void),
                      void (*rtld_fini)(void),
                      void *stack_end) {
  (void)fini;
  (void)rtld_fini;
  (void)stack_end;
  if (init) {
    init();
  }
  if (fp_shim_trace_enabled()) {
    const char *arg0 = (argc > 0 && argv && argv[0]) ? argv[0] : "(null)";
    dprintf(2, "[fp-shim] __libc_start_main argc=%d argv0='%s'\n", argc, arg0);
  }
  int rc = 0;
  if (main_fn) {
    rc = main_fn(argc, argv, environ);
  }
  exit(rc);
}

__attribute__((visibility("default")))
int __overflow(FILE *stream, int ch) {
  return fputc(ch, stream);
}

__attribute__((visibility("default")))
int fflush_unlocked(FILE *stream) {
  return fflush(stream);
}

__attribute__((visibility("default")))
int fputc_unlocked(int ch, FILE *stream) {
  return fputc(ch, stream);
}

__attribute__((visibility("default")))
int fputs_unlocked(const char *s, FILE *stream) {
  return fputs(s, stream);
}

__attribute__((visibility("default")))
int fgetc_unlocked(FILE *stream) {
  return fgetc(stream);
}

__attribute__((visibility("default")))
int getc_unlocked(FILE *stream) {
  return getc(stream);
}

__attribute__((visibility("default")))
int putc_unlocked(int ch, FILE *stream) {
  return putc(ch, stream);
}

__attribute__((visibility("default")))
size_t fwrite_unlocked(const void *ptr, size_t size, size_t nmemb, FILE *stream) {
  return fwrite(ptr, size, nmemb, stream);
}

__attribute__((visibility("default")))
size_t fread_unlocked(void *ptr, size_t size, size_t nmemb, FILE *stream) {
  return fread(ptr, size, nmemb, stream);
}

__attribute__((visibility("default")))
ssize_t lgetxattr(const char *path,
                  const char *name,
                  void *value,
                  size_t size) {
  return getxattr(path, name, value, size, 0, XATTR_NOFOLLOW);
}

__attribute__((visibility("default")))
ssize_t llistxattr(const char *path, char *list, size_t size) {
  return listxattr(path, list, size, XATTR_NOFOLLOW);
}

__attribute__((visibility("default")))
size_t mbrtoc32(char32_t *pc32, const char *s, size_t n, mbstate_t *ps) {
  (void)ps;

  if (s == NULL) {
    return 0;
  }
  if (n == 0) {
    return (size_t)-2;
  }

  unsigned char ch = (unsigned char)s[0];
  if (ch == 0) {
    if (pc32) {
      *pc32 = 0;
    }
    return 0;
  }
  if (ch < 0x80) {
    if (pc32) {
      *pc32 = (char32_t)ch;
    }
    return 1;
  }

  errno = EILSEQ;
  return (size_t)-1;
}

__attribute__((visibility("default")))
char *__progname = "fp";

__attribute__((visibility("default")))
char *__progname_full = "fp";

extern FILE *__stdinp;
extern FILE *__stdoutp;
extern FILE *__stderrp;

__attribute__((visibility("default")))
FILE *stdin;

__attribute__((visibility("default")))
FILE *stdout;

__attribute__((visibility("default")))
FILE *stderr;

__attribute__((constructor))
static void fp_init_stdio_globals(void) {
  stdin = __stdinp;
  stdout = __stdoutp;
  stderr = __stderrp;
}

__attribute__((visibility("default")))
char *bindtextdomain(const char *domainname, const char *dirname) {
  (void)domainname;
  return (char *)dirname;
}

__attribute__((visibility("default")))
char *dcgettext(const char *domainname, const char *msgid, int category) {
  (void)domainname;
  (void)category;
  return (char *)msgid;
}

__attribute__((visibility("default")))
char *textdomain(const char *domainname) {
  return (char *)domainname;
}

__attribute__((visibility("default")))
char *gettext(const char *msgid) {
  return (char *)msgid;
}

__attribute__((visibility("default")))
char *dgettext(const char *domainname, const char *msgid) {
  (void)domainname;
  return (char *)msgid;
}

__attribute__((visibility("default")))
void *cap_get_file(const char *path) {
  (void)path;
  return NULL;
}

__attribute__((visibility("default")))
char *cap_to_text(void *cap, ssize_t *len_p) {
  (void)cap;
  if (len_p) {
    *len_p = 0;
  }
  return NULL;
}

__attribute__((visibility("default")))
int cap_free(void *cap) {
  (void)cap;
  return 0;
}

// --- Linux ABI shims (struct layout translations) ---
//
// When running Linux-built binaries on macOS we cannot forward functions like
// `stat()` or `readdir()` directly to libSystem because the caller expects
// Linux/glibc struct layouts in memory.

typedef struct {
  int64_t tv_sec;
  int64_t tv_nsec;
} fp_linux_timespec;

typedef struct {
  uint64_t st_dev;
  uint64_t st_ino;
  uint64_t st_nlink;
  uint32_t st_mode;
  uint32_t st_uid;
  uint32_t st_gid;
  int32_t __pad0;
  uint64_t st_rdev;
  int64_t st_size;
  int64_t st_blksize;
  int64_t st_blocks;
  fp_linux_timespec st_atim;
  fp_linux_timespec st_mtim;
  fp_linux_timespec st_ctim;
  int64_t __glibc_reserved[3];
} fp_linux_stat;

_Static_assert(offsetof(fp_linux_stat, st_rdev) == 40, "linux stat layout");
_Static_assert(sizeof(fp_linux_stat) == 144, "linux stat size");

static void fp_linux_stat_from_darwin(fp_linux_stat *out, const struct stat *in) {
  memset(out, 0, sizeof(*out));
  out->st_dev = (uint64_t)in->st_dev;
  out->st_ino = (uint64_t)in->st_ino;
  out->st_nlink = (uint64_t)in->st_nlink;
  out->st_mode = (uint32_t)in->st_mode;
  out->st_uid = (uint32_t)in->st_uid;
  out->st_gid = (uint32_t)in->st_gid;
  out->st_rdev = (uint64_t)in->st_rdev;
  out->st_size = (int64_t)in->st_size;
  out->st_blksize = (int64_t)in->st_blksize;
  out->st_blocks = (int64_t)in->st_blocks;

#if defined(__APPLE__)
  out->st_atim.tv_sec = (int64_t)in->st_atimespec.tv_sec;
  out->st_atim.tv_nsec = (int64_t)in->st_atimespec.tv_nsec;
  out->st_mtim.tv_sec = (int64_t)in->st_mtimespec.tv_sec;
  out->st_mtim.tv_nsec = (int64_t)in->st_mtimespec.tv_nsec;
  out->st_ctim.tv_sec = (int64_t)in->st_ctimespec.tv_sec;
  out->st_ctim.tv_nsec = (int64_t)in->st_ctimespec.tv_nsec;
#else
  out->st_atim.tv_sec = (int64_t)in->st_atim.tv_sec;
  out->st_atim.tv_nsec = (int64_t)in->st_atim.tv_nsec;
  out->st_mtim.tv_sec = (int64_t)in->st_mtim.tv_sec;
  out->st_mtim.tv_nsec = (int64_t)in->st_mtim.tv_nsec;
  out->st_ctim.tv_sec = (int64_t)in->st_ctim.tv_sec;
  out->st_ctim.tv_nsec = (int64_t)in->st_ctim.tv_nsec;
#endif
}

typedef int (*fp_host_stat_fn)(const char *, struct stat *);
typedef int (*fp_host_lstat_fn)(const char *, struct stat *);
typedef int (*fp_host_fstat_fn)(int, struct stat *);
typedef int (*fp_host_fstatat_fn)(int, const char *, struct stat *, int);

static fp_host_stat_fn fp_host_stat(void) {
  static fp_host_stat_fn fn;
  if (!fn) {
    fn = (fp_host_stat_fn)dlsym(RTLD_NEXT, "stat");
  }
  return fn;
}

static fp_host_lstat_fn fp_host_lstat(void) {
  static fp_host_lstat_fn fn;
  if (!fn) {
    fn = (fp_host_lstat_fn)dlsym(RTLD_NEXT, "lstat");
  }
  return fn;
}

static fp_host_fstat_fn fp_host_fstat(void) {
  static fp_host_fstat_fn fn;
  if (!fn) {
    fn = (fp_host_fstat_fn)dlsym(RTLD_NEXT, "fstat");
  }
  return fn;
}

static fp_host_fstatat_fn fp_host_fstatat(void) {
  static fp_host_fstatat_fn fn;
  if (!fn) {
    fn = (fp_host_fstatat_fn)dlsym(RTLD_NEXT, "fstatat");
  }
  return fn;
}

__attribute__((visibility("default")))
int stat(const char *path, struct stat *out) {
  if (!out) {
    errno = EFAULT;
    return -1;
  }
  fp_linux_stat *linux_out = (fp_linux_stat *)out;
  struct stat st;
  fp_host_stat_fn fn = fp_host_stat();
  if (!fn) {
    errno = ENOSYS;
    return -1;
  }
  int rc = fn(path, &st);
  if (rc != 0) {
    return rc;
  }
  fp_linux_stat_from_darwin(linux_out, &st);
  if (fp_shim_trace_enabled()) {
    dprintf(2, "[fp-shim] stat('%s') -> dev=%" PRIu64 " ino=%" PRIu64 "\n", path,
            linux_out->st_dev, linux_out->st_ino);
  }
  return 0;
}

__attribute__((visibility("default")))
int lstat(const char *path, struct stat *out) {
  if (!out) {
    errno = EFAULT;
    return -1;
  }
  fp_linux_stat *linux_out = (fp_linux_stat *)out;
  struct stat st;
  fp_host_lstat_fn fn = fp_host_lstat();
  if (!fn) {
    errno = ENOSYS;
    return -1;
  }
  int rc = fn(path, &st);
  if (rc != 0) {
    return rc;
  }
  fp_linux_stat_from_darwin(linux_out, &st);
  return 0;
}

__attribute__((visibility("default")))
int fstat(int fd, struct stat *out) {
  if (!out) {
    errno = EFAULT;
    return -1;
  }
  fp_linux_stat *linux_out = (fp_linux_stat *)out;
  struct stat st;
  fp_host_fstat_fn fn = fp_host_fstat();
  if (!fn) {
    errno = ENOSYS;
    return -1;
  }
  int rc = fn(fd, &st);
  if (rc != 0) {
    return rc;
  }
  fp_linux_stat_from_darwin(linux_out, &st);
  if (fp_shim_trace_enabled()) {
    dprintf(2, "[fp-shim] fstat(%d) -> dev=%" PRIu64 " ino=%" PRIu64 "\n", fd,
            linux_out->st_dev, linux_out->st_ino);
  }
  return 0;
}

__attribute__((visibility("default")))
int fstatat(int dirfd, const char *path, struct stat *out, int flags) {
  if (!out) {
    errno = EFAULT;
    return -1;
  }
  fp_linux_stat *linux_out = (fp_linux_stat *)out;
  struct stat st;
  fp_host_fstatat_fn fn = fp_host_fstatat();
  if (!fn) {
    errno = ENOSYS;
    return -1;
  }
  int rc = fn(dirfd, path, &st, flags);
  if (rc != 0) {
    return rc;
  }
  fp_linux_stat_from_darwin(linux_out, &st);
  return 0;
}

__attribute__((visibility("default")))
int __xstat(int ver, const char *path, struct stat *out) {
  (void)ver;
  return stat(path, out);
}

__attribute__((visibility("default")))
int __lxstat(int ver, const char *path, struct stat *out) {
  (void)ver;
  return lstat(path, out);
}

__attribute__((visibility("default")))
int __fxstat(int ver, int fd, struct stat *out) {
  (void)ver;
  return fstat(fd, out);
}

__attribute__((visibility("default")))
int __fxstatat(int ver, int dirfd, const char *path, struct stat *out, int flags) {
  (void)ver;
  return fstatat(dirfd, path, out, flags);
}

typedef struct {
  uint64_t d_ino;
  int64_t d_off;
  uint16_t d_reclen;
  uint8_t d_type;
  char d_name[256];
} fp_linux_dirent;

_Static_assert(offsetof(fp_linux_dirent, d_name) == 19, "linux dirent layout");

typedef void *(*fp_host_opendir_fn)(const char *);
typedef struct dirent *(*fp_host_readdir_fn)(void *);
typedef int (*fp_host_closedir_fn)(void *);
typedef int (*fp_host_dirfd_fn)(void *);

static fp_host_opendir_fn fp_host_opendir(void) {
  static fp_host_opendir_fn fn;
  if (!fn) {
    fn = (fp_host_opendir_fn)dlsym(RTLD_NEXT, "opendir");
  }
  return fn;
}

static fp_host_readdir_fn fp_host_readdir(void) {
  static fp_host_readdir_fn fn;
  if (!fn) {
    fn = (fp_host_readdir_fn)dlsym(RTLD_NEXT, "readdir");
  }
  return fn;
}

static fp_host_closedir_fn fp_host_closedir(void) {
  static fp_host_closedir_fn fn;
  if (!fn) {
    fn = (fp_host_closedir_fn)dlsym(RTLD_NEXT, "closedir");
  }
  return fn;
}

static fp_host_dirfd_fn fp_host_dirfd(void) {
  static fp_host_dirfd_fn fn;
  if (!fn) {
    fn = (fp_host_dirfd_fn)dlsym(RTLD_NEXT, "dirfd");
  }
  return fn;
}

typedef struct {
  DIR *host_dir;
  fp_linux_dirent entry;
} fp_linux_dir;

static int fp_open_dir_count = 0;

__attribute__((visibility("default")))
DIR *opendir(const char *name) {
  fp_host_opendir_fn fn = fp_host_opendir();
  if (!fn) {
    errno = ENOSYS;
    return NULL;
  }
  DIR *host_dir = (DIR *)fn(name);
  if (!host_dir) {
    return NULL;
  }
  fp_linux_dir *dir = (fp_linux_dir *)calloc(1, sizeof(*dir));
  if (!dir) {
    fp_host_closedir_fn close_fn = fp_host_closedir();
    if (close_fn) {
      close_fn(host_dir);
    }
    errno = ENOMEM;
    return NULL;
  }
  dir->host_dir = host_dir;
  fp_open_dir_count++;
  if (fp_shim_trace_enabled()) {
    dprintf(2, "[fp-shim] opendir('%s') -> %p (open=%d)\n", name, (void *)dir,
            fp_open_dir_count);
  }
  return (DIR *)dir;
}

__attribute__((visibility("default")))
struct dirent *readdir(DIR *dirp) {
  fp_linux_dir *dir = (fp_linux_dir *)dirp;
  if (!dir || !dir->host_dir) {
    errno = EBADF;
    return NULL;
  }
  fp_host_readdir_fn fn = fp_host_readdir();
  if (!fn) {
    errno = ENOSYS;
    return NULL;
  }
  struct dirent *ent = fn(dir->host_dir);
  if (!ent) {
    return NULL;
  }
  memset(&dir->entry, 0, sizeof(dir->entry));
  dir->entry.d_ino = (uint64_t)ent->d_ino;
  dir->entry.d_type = (uint8_t)ent->d_type;
  // `readdir()` returns a pointer to storage owned by the directory stream.
  // Provide a fixed-size, NUL-terminated name buffer.
  strncpy(dir->entry.d_name, ent->d_name, sizeof(dir->entry.d_name) - 1);
  dir->entry.d_name[sizeof(dir->entry.d_name) - 1] = '\0';
  size_t name_len = strnlen(dir->entry.d_name, sizeof(dir->entry.d_name));
  size_t reclen = offsetof(fp_linux_dirent, d_name) + name_len + 1;
  if (reclen > UINT16_MAX) {
    reclen = UINT16_MAX;
  }
  dir->entry.d_reclen = (uint16_t)reclen;

  // This is a best-effort mapping. Darwin uses a different directory stream
  // implementation, so `d_off` is not always meaningful.
  dir->entry.d_off = (int64_t)ent->d_seekoff;

  if (fp_shim_trace_enabled()) {
    dprintf(2, "[fp-shim] readdir(%p) -> '%s'\n", (void *)dir, dir->entry.d_name);
  }

  return (struct dirent *)&dir->entry;
}

__attribute__((visibility("default")))
int closedir(DIR *dirp) {
  fp_linux_dir *dir = (fp_linux_dir *)dirp;
  if (!dir) {
    errno = EBADF;
    return -1;
  }
  int rc = 0;
  if (dir->host_dir) {
    fp_host_closedir_fn fn = fp_host_closedir();
    if (!fn) {
      errno = ENOSYS;
      rc = -1;
    } else {
      rc = fn(dir->host_dir);
    }
  }
  fp_open_dir_count--;
  if (fp_shim_trace_enabled()) {
    dprintf(2, "[fp-shim] closedir(%p) -> %d (open=%d)\n", (void *)dir, rc,
            fp_open_dir_count);
  }
  free(dir);
  return rc;
}

__attribute__((visibility("default")))
int dirfd(DIR *dirp) {
  fp_linux_dir *dir = (fp_linux_dir *)dirp;
  if (!dir || !dir->host_dir) {
    errno = EBADF;
    return -1;
  }
  fp_host_dirfd_fn fn = fp_host_dirfd();
  if (!fn) {
    errno = ENOSYS;
    return -1;
  }
  return fn(dir->host_dir);
}

