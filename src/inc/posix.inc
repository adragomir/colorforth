%ifndef _POSIX_INC_
%define _POSIX_INC_

%include "macros.inc"

extern _puts
%macro	__puts	1
    ccall	_puts, %1
%endmacro	;__puts

extern _sprintf
%macro	__sprintf 2-*
    ccall	_sprintf, %1, %2, %3
%endmacro	;__sprintf

extern _atexit
%macro		__atexit 1 ; proc
  ccall _atexit, %1
%endmacro	;__sys_exit

; syscalls
%define	SYS_syscall        0
%define	SYS_exit           1
%define	SYS_fork           2
%define	SYS_read           3
%define	SYS_lseek          199
%define	SYS_truncate       200
%define	SYS_ftruncate      201
%define	SYS_write          4
%define	SYS_open           5
%define	SYS_close          6
%define	SYS_wait4          7
%define	SYS_chdir          12
%define	SYS_fchdir         13
%define	SYS_mknod          14
%define	SYS_chmod          15
%define	SYS_chown          16
%define	SYS_munmap         73
%define	SYS_mprotect       74
%define	SYS_madvise        75
%define	SYS_setprivexec    152
%define	SYS_pread          153
%define	SYS_pwrite         154
%define	SYS_mmap           197
%define	SYS_stat           188
%define	SYS_fstat          189

;;;;;;; open flags
%define	O_RDONLY	0
%define	O_WRONLY	1
%define	O_RDWR	2

;;;;;;; mmap flags
%define	PROT_NONE	0
%define	PROT_READ	1
%define	PROT_WRITE	2
%define	PROT_EXEC	4
;;
%define	MAP_SHARED	0001h
%define	MAP_PRIVATE	0002h

struct stat
  mdword st_dev
  mdword st_ino
  mword st_mode
  mword st_nlink
  mdword st_uid
  mdword st_gid
  mdword st_rdev
  mdword st_atime
  mdword st_atimensec
  mdword st_mtime
  mdword st_mtimensec
  mdword st_ctime
  mdword st_ctimensec
  mdword st_size
  mqword st_blocks
  mdword st_blksize
  mdword st_flags
  mdword st_gen
  mdword st_lspare
  mdword st_qspare
ends

%endif
