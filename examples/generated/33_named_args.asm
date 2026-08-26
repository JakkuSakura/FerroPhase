fp-native dump: format=MachO arch=Aarch64 entry=0x60

AsmIR:
asmir target=Aarch64 format=MachO endian=Little ptr=64
section .text kind=Text align=Some(16)
section .rodata kind=ReadOnlyData align=Some(16)
global __const_data_0 ty=Array(I8, 6) constant=true initializer=Some(Bytes([97, 108, 112, 104, 97, 0]))
global __const_data_1 ty=Array(I8, 5) constant=true initializer=Some(Bytes([98, 101, 116, 97, 0]))
fn realloc
fn munlockall
fn setitimer
fn strdup
fn getaddrinfo
fn thread_get_mach_voucher
fn vswscanf
fn pwrite
fn sigemptyset
fn getpriority
fn mblen
fn iswpunct
fn rewinddir
fn sem_init
fn getppid
fn seteuid
fn task_set_phys_footprint_limit
fn host_set_special_port
fn feraiseexcept
fn setservent
fn fputc
fn posix_spawn
fn processor_set_max_priority
fn NSSymbolReferenceNameInObjectFileImage
fn wcsncat
fn NSLookupAndBindSymbolWithHint
fn vm_allocate
fn iswrune
fn NSAddLibraryWithSearching
fn mknod
fn task_assign_default
fn sync
fn mach_port_guard_with_flags
fn _kernelrpc_mach_port_mod_refs_trap
fn waitid
fn gethostbyaddr
fn swtch_pri
fn mach_msg
fn getpwuid
fn memcpy
fn getsid
fn thread_terminate
fn mach_memory_object_memory_entry
fn NSLookupSymbolInModule
fn insque
fn fclonefileat
fn task_policy
fn mach_port_get_context
fn posix_spawnattr_getsigmask
fn accept
fn NSNameOfSymbol
fn creat
fn hsearch
fn processor_set_destroy
fn vm_map_64
fn aio_suspend
fn execvp
fn wctype
fn getlogin
fn mig_deallocate
fn strcspn
fn processor_info
fn host_register_well_known_mach_voucher_attr_manager
fn act_set_state
fn endhostent
fn _dyld_present
fn getnetbyaddr
fn wait
fn ctime
fn execv
fn sleep
fn task_resume2
fn iswprint
fn mach_port_allocate_name
fn kmod_get_info
fn NXSwapHostShortToBig
fn __error
fn raise
fn _NSGetExecutablePath
fn strftime
fn getc_unlocked
fn shutdown
fn inet_addr
fn NXSwapBigIntToHost
fn fstatat
fn mach_port_swap_guard
fn debug_control_port_for_pid
fn tempnam
fn swab
fn mach_port_mod_refs
fn NSGetSectionDataInObjectFileImage
fn getnameinfo
fn getrlimit
fn fstatvfs
fn msync
fn iscntrl
fn fread
fn setgrfile
fn NSDestroyObjectFileImage
fn putenv
fn ctermid
fn posix_spawnattr_setsigdefault
fn sigsuspend
fn quick_exit
fn posix_spawn_file_actions_addopen
fn getcwd
fn mig_dealloc_reply_port
fn thread_assign_default
fn rand
fn gethostid
fn strcat
fn recvfrom
fn mach_port_get_refs
fn vm_read_list
fn aio_cancel
fn inet_pton
fn task_policy_set
fn strtoul
fn nanosleep
fn readlinkat
fn listen
fn wcscpy
fn getuid
fn ldiv
fn msgsnd
fn _kernelrpc_mach_port_request_notification_trap
fn host_check_multiuser_mode
fn pread
fn sigismember
fn isascii
fn random
fn _OSReadSwapInt32
fn task_identity_token_get_task_port
fn NSModuleForSymbol
fn ffs
fn thread_get_exception_ports
fn setnetent
fn host_reboot
fn mach_port_extract_member
fn mig_strncpy
fn vm_msync
fn thread_policy_set
fn _setjmp
fn strtoumax
fn fork
fn nl_langinfo
fn getpwuid_r
fn gettimeofday
fn mig_get_reply_port
fn thread_adopt_exception_handler
fn _kernelrpc_mach_port_move_member_trap
fn sigpending
fn mach_port_request_notification
fn wcstoumax
fn atomic_signal_fence
fn putc
fn sigignore
fn pthread_testcancel
fn vprintf
fn _Exit
fn iswdigit
fn lockf
fn __darwin_fd_set
fn mbsnrtowcs
fn sysconf
fn processor_set_tasks_with_flavor
fn getsockname
fn vwprintf
fn wcswidth
fn getgrgid_r
fn utimensat
fn __istype
fn wcsnlen
fn _OSReadSwapInt64
fn host_priv_statistics
fn thread_swap_exception_ports
fn task_self_trap
fn NSInstallLinkEditErrorHandlers
fn kqueue
fn msgrcv
fn wmemcpy
fn NSIsSymbolNameDefinedInImage
fn wcstoll
fn srandom
fn select
fn getpeername
fn malloc
fn dlsym
fn semaphore_signal
fn mbstowcs
fn wcrtomb
fn isdigit
fn processor_get_assignment
fn host_get_clock_service
fn memset
fn strerror
fn shmat
fn _kernelrpc_mach_port_deallocate_trap
fn openlog
fn gai_strerror
fn pid_for_task
fn readlink
fn semaphore_wait_signal
fn __wcwidth
fn open_memstream
fn strncat
fn endservent
fn __darwin_fd_clr
fn _OSReadInt32
fn mach_memory_object_memory_entry_64
fn __math_errhandling
fn host_statistics
fn NXSwapHostLongLongToLittle
fn getprotobynumber
fn getlogin_r
fn mach_ports_register
fn sem_wait
fn _dyld_launched_prebound
fn getentropy
fn aio_read
fn fmemopen
fn freopen
fn host_processor_set_priv
fn sockatmark
fn dup
fn getline
fn wcsncpy
fn __NDR_convert__mig_reply_error_t
fn wcstol
fn wcscat
fn getdate
fn sigdelset
fn vsprintf
fn futimens
fn _OSSwapInt64
fn wcsrtombs
fn dirname
fn clock_set_time
fn tcgetpgrp
fn getprotoent
fn kmod_create
fn mach_port_get_srights
fn NSVersionOfLinkTimeLibrary
fn task_register_dyld_shared_cache_image_info
fn task_get_assignment
fn NXSwapLittleIntToHost
fn _kernelrpc_mach_port_construct_trap
fn towlower
fn endgrent
fn dup2
fn vm_map
fn host_get_boot_info
fn access
fn vm_region_64
fn mach_port_is_connection_for_service
fn OSHostByteOrder
fn act_get_state
fn mach_port_allocate_full
fn clearerr
fn getsubopt
fn strtok
fn processor_set_statistics
fn thread_swap_mach_voucher
fn readdir
fn wmemchr
fn memcmp
fn aio_write
fn endpwent
fn getnetent
fn setpriority
fn setjmp
fn hdestroy
fn posix_spawnattr_destroy
fn close
fn pipe
fn mbrlen
fn mach_vm_region_info_64
fn mach_port_kernel_object
fn mach_port_set_context
fn mbsinit
fn vm_read
fn mach_port_assert_attributes
fn _dyld_lookup_and_bind
fn posix_spawnattr_init
fn posix_openpt
fn thread_set_state
fn vm_behavior_set
fn putwc
fn mach_port_destroy
fn mach_port_guard
fn aio_return
fn task_get_exception_ports
fn jrand48
fn time
fn wcslen
fn wcsxfrm
fn gethostbyname
fn tcgetattr
fn strxfrm
fn getitimer
fn host_processor_sets
fn task_set_corpse_forking_behavior
fn thread_set_mach_voucher
fn kmod_destroy
fn waitpid
fn aio_error
fn NXSwapBigLongLongToHost
fn towupper
fn voucher_mach_msg_set
fn getprotobyname
fn sched_yield
fn wcscmp
fn isupper
fn remque
fn mig_strncpy_zerofill
fn pause
fn mach_port_allocate
fn mach_port_get_set_status
fn setegid
fn setvbuf
fn __svfscanf
fn iswctype
fn utime
fn munlock
fn grantpt
fn processor_set_threads
fn cfgetispeed
fn mknodat
fn semaphore_wait
fn stpncpy
fn task_set_exc_guard_behavior
fn asctime_r
fn connect
fn readdir_r
fn thread_sample
fn task_register_dyld_image_infos
fn ctime_r
fn shmdt
fn kmod_control
fn send
fn vswprintf
fn tcsetattr
fn host_get_UNDServer
fn removexattr
fn sched_get_priority_max
fn pthread_sigmask
fn __maskrune
fn gets
fn system
fn setlogmask
fn task_map_corpse_info
fn if_indextoname
fn task_map_corpse_info_64
fn macx_swapon
fn processor_set_stack_usage
fn iswspecial
fn fputws
fn task_get_exception_ports_info
fn mach_port_insert_member
fn task_dyld_process_info_notify_get
fn task_swap_mach_voucher
fn fgetws
fn _kernelrpc_mach_port_extract_member_trap
fn longjmp
fn abort
fn initstate
fn NXSwapShort
fn isblank
fn tmpnam
fn wcscoll
fn wcsstr
fn task_register_dyld_set_dyld_state
fn fgetpos
fn iswxdigit
fn __vsnprintf_chk
fn open_wmemstream
fn posix_spawnattr_getpgroup
fn realpath
fn setpgid
fn putc_unlocked
fn timespec_get
fn symlinkat
fn host_set_UNDServer
fn fsync
fn stpcpy
fn fseeko
fn setreuid
fn killpg
fn iswblank
fn _OSWriteSwapInt64
fn semaphore_timedwait_signal
fn task_suspend
fn sigaltstack
fn mach_make_memory_entry
fn macx_backing_store_recovery
fn host_virtual_physical_table_info
fn if_nameindex
fn mig_reply_setup
fn NXSwapHostIntToLittle
fn slot_name
fn thread_get_special_port
fn fegetexceptflag
fn NXSwapLittleLongLongToHost
fn clonefile
fn imaxdiv
fn ___tolower
fn strpbrk
fn mach_port_dnrequest_info
fn _kernelrpc_mach_port_destruct_trap
fn strtoimax
fn vscanf
fn strncpy
fn voucher_mach_msg_adopt
fn mach_voucher_extract_attr_recipe_trap
fn setgrent
fn thread_set_policy
fn task_assign
fn wcstoimax
fn mach_port_set_mscount
fn wcsrchr
fn cfsetispeed
fn puts
fn closedir
fn _OSWriteInt32
fn task_set_policy
fn vm_protect
fn labs
fn ftello
fn remove
fn read
fn host_register_mach_voucher_attr_manager
fn _dyld_lookup_and_bind_fully
fn mach_vm_reclaim_update_kernel_accounting_trap
fn _host_page_size
fn strptime
fn iswalnum
fn getopt
fn wcstombs
fn task_policy_get
fn mktime
fn processor_control
fn _dyld_get_image_header_containing_address
fn gmtime
fn thread_depress_abort
fn uselocale
fn host_get_multiuser_config_flags
fn strstr
fn sem_trywait
fn thread_info
fn vm_remap_new
fn setlocale
fn mach_memory_info
fn mbtowc
fn tcdrain
fn NSSymbolDefinitionCountInObjectFileImage
fn task_get_dyld_image_infos
fn _dyld_shared_cache_contains_path
fn tcsendbreak
fn strchr
fn truncate
fn vm_remap
fn dlclose
fn l64a
fn fesetround
fn newlocale
fn wcwidth
fn vwscanf
fn iconv_close
fn task_inspect
fn NXSwapHostShortToLittle
fn rand_r
fn inet_ntop
fn clock_getres
fn tcflow
fn task_dyld_process_info_notify_register
fn NSIsSymbolDefinedInObjectFileImage
fn freeaddrinfo
fn _kernelrpc_mach_port_allocate_trap
fn fremovexattr
fn sethostent
fn getpgrp
fn vsnprintf
fn host_get_special_port
fn mach_msg_receive
fn voucher_mach_msg_revert
fn link
fn strtoull
fn task_threads
fn host_kernel_version
fn lio_listio
fn processor_set_policy_control
fn mach_port_peek
fn vm_region_recurse
fn aligned_alloc
fn posix_spawnattr_getsigdefault
fn wcschr
fn endnetent
fn crypt
fn rename
fn wcpcpy
fn sigrelse
fn __vsprintf_chk
fn unlinkat
fn strtok_r
fn fstat
fn mktemp
fn task_set_special_port
fn atomic_flag_test_and_set
fn free
fn ftok
fn mkfifo
fn mach_voucher_deallocate
fn host_security_set_task_token
fn wcsdup
fn vfscanf
fn sem_close
fn semaphore_signal_all
fn chown
fn task_zone_info
fn mach_port_allocate_qos
fn clock_sleep_trap
fn islower
fn posix_spawn_file_actions_addfchdir
fn vm_wire
fn mach_port_names
fn getgrnam_r
fn fopen
fn getgroups
fn semget
fn _OSWriteInt16
fn clock_set_res
fn NXSwapLong
fn NXSwapHostLongToLittle
fn NSCreateObjectFileImageFromMemory
fn NXSwapBigShortToHost
fn isalnum
fn _dyld_get_image_vmaddr_slide
fn sigaction
fn getchar
fn pthread_kill
fn lstat
fn statvfs
fn task_set_mach_voucher
fn isgraph
fn task_generate_corpse
fn mach_port_get_service_port_info
fn NXSwapDouble
fn processor_set_create
fn getpgid
fn calloc
fn vm_allocate_cpm
fn mach_error
fn _dyld_get_image_header
fn host_get_exception_ports
fn NSUnLinkModule
fn strncasecmp
fn strrchr
fn llabs
fn strerror_r
fn cfsetospeed
fn getenv
fn fegetround
fn confstr
fn task_set_info
fn vm_deallocate
fn mach_port_unguard
fn _kernelrpc_mach_port_get_attributes_trap
fn posix_memalign
fn __isctype
fn siglongjmp
fn setprotoent
fn munmap
fn _kernelrpc_mach_port_guard_trap
fn _dyld_bind_fully_image_containing_address
fn unlockpt
fn asctime
fn iswgraph
fn wcscasecmp
fn wcsncasecmp
fn socketpair
fn atomic_flag_test_and_set_explicit
fn getegid
fn mbrtowc
fn mig_put_reply_port
fn NSVersionOfRunTimeLibrary
fn sigfillset
fn sighold
fn fseek
fn isatty
fn pathconf
fn wcscspn
fn mach_port_rename
fn NSIsSymbolNameDefinedWithHint
fn sendto
fn fgetxattr
fn setsid
fn task_get_exc_guard_behavior
fn task_set_emulation
fn mach_port_set_attributes
fn __assert_rtn
fn sigpause
fn strlen
fn getwchar
fn task_terminate
fn ptsname
fn mach_port_space_basic_info
fn task_unregister_dyld_image_infos
fn _OSWriteInt64
fn lrand48
fn getgrgid
fn fpathconf
fn semop
fn processor_set_info
fn thread_create_running
fn _kernelrpc_mach_vm_map_trap
fn pthread_setconcurrency
fn mach_msg_send
fn regfree
fn gethostname
fn symlink
fn host_statistics64
fn wctomb
fn posix_spawnattr_getflags
fn getgrnam
fn ferror
fn fileno
fn __srget
fn getdelim
fn wcstok
fn isxdigit
fn fgetc
fn ungetc
fn _OSSwapInt32
fn fnmatch
fn globfree
fn posix_spawnattr_setpgroup
fn tcsetpgrp
fn mkdir
fn kext_request
fn processor_exit
fn task_suspend2
fn hcreate
fn __sigbits
fn fputwc
fn getwc
fn setgid
fn mach_vm_wire
fn vm_inherit
fn vfwprintf
fn mach_zone_info
fn task_name_for_pid
fn abs
fn task_wire
fn macx_triggers
fn NXSwapHostLongLongToBig
fn wctob
fn getpid
fn localtime
fn putchar_unlocked
fn vm_stats
fn write
fn shmget
fn setpwent
fn vm_write
fn siginterrupt
fn mach_port_extract_right
fn feholdexcept
fn __darwin_check_fd_set_overflow
fn posix_spawn_file_actions_destroy
fn processor_set_policy_enable
fn task_test_async_upcall_propagation
fn pselect
fn semaphore_timedwait
fn etap_trace_thread
fn mach_port_get_attributes
fn host_create_mach_voucher
fn _kernelrpc_mach_vm_protect_trap
fn NXSwapLittleShortToHost
fn seekdir
fn NSLookupSymbolInImage
fn kevent
fn NSNameOfModule
fn thread_convert_thread_state
fn NSLookupAndBindSymbol
fn ispunct
fn NSAddLibrary
fn vfprintf
fn mkdirat
fn task_create_identity_token
fn fwrite
fn alarm
fn NXSwapLittleLongToHost
fn _longjmp
fn sigprocmask
fn clock_settime
fn feupdateenv
fn iswhexnumber
fn getservbyport
fn fetestexcept
fn posix_spawn_file_actions_init
fn host_get_clock_control
fn mrand48
fn perror
fn lock_set_destroy
fn host_default_memory_manager
fn gmtime_r
fn thread_policy
fn mach_port_kobject_description
fn task_swap_exception_ports
fn NXSwapFloat
fn encrypt
fn ttyname_r
fn thread_abort_safely
fn umask
fn mach_port_space_info
fn mach_host_self
fn task_set_emulation_vector
fn isspace
fn fesetexceptflag
fn fputs
fn lchown
fn ualarm
fn inet_ntoa
fn task_dyld_process_info_notify_deregister
fn dlopen
fn basename
fn posix_spawnattr_setflags
fn NSSymbolReferenceCountInObjectFileImage
fn fegetenv
fn utimes
fn vfwscanf
fn NSCreateObjectFileImageFromFile
fn NSSymbolDefinitionNameInObjectFileImage
fn fflush
fn setkey
fn memmove
fn sigsetjmp
fn getpwnam
fn posix_spawn_file_actions_addchdir
fn tcflush
fn rmdir
fn regexec
fn __darwin_check_fd_set
fn clock_sleep
fn clock_gettime
fn getgid
fn setregid
fn iswupper
fn memchr
fn host_security_create_task_token
fn _kernelrpc_mach_vm_purgable_control_trap
fn NSLibraryNameForModule
fn getc
fn isalpha
fn wcstoull
fn strsignal
fn regerror
fn fchdir
fn fgets
fn wcsftime
fn mach_port_deallocate
fn msgctl
fn _kernelrpc_mach_port_type_trap
fn swtch
fn panic_init
fn freelocale
fn getgrent
fn _OSReadSwapInt16
fn task_purgable_info
fn vm_map_exec_lockdown
fn NXSwapInt
fn NXSwapBigLongToHost
fn strcpy
fn putwchar
fn listxattr
fn nice
fn cfgetospeed
fn lseek
fn iswideogram
fn __swbuf
fn vfork
fn strcmp
fn task_get_state
fn thread_policy_get
fn vm_machine_attribute
fn vm_region
fn task_set_ras_pc
fn _dyld_lookup_and_bind_with_hint
fn setxattr
fn _kernelrpc_mach_port_insert_right_trap
fn sched_get_priority_min
fn iswcntrl
fn task_test_sync_upcall
fn NSAddressOfSymbol
fn toupper
fn fdopendir
fn btowc
fn dlerror
fn semaphore_create
fn setsockopt
fn ttyname
fn clock_set_attributes
fn semaphore_destroy
fn task_set_state
fn mach_generate_activity_id
fn ftell
fn div
fn a64l
fn mach_msg_overwrite
fn thread_set_special_port
fn posix_madvise
fn mach_zone_info_for_zone
fn pclose
fn __tolower
fn sem_unlink
fn faccessat
fn regcomp
fn NXSwapLongLong
fn iconv
fn strcasecmp
fn atoi
fn task_set_exception_ports
fn iswalpha
fn thread_switch
fn fgetwc
fn getchar_unlocked
fn ungetwc
fn lock_set_create
fn _OSSwapInt16
fn tcgetsid
fn opendir
fn geteuid
fn fesetenv
fn vsscanf
fn mach_error_string
fn mach_error_type
fn host_lockgroup_info
fn host_processors
fn task_sample
fn mkstemp
fn strcoll
fn clock_get_res
fn wcspbrk
fn rewind
fn _dyld_image_containing_address
fn processor_set_default
fn _exit
fn shmctl
fn getpwnam_r
fn chdir
fn mach_ports_lookup
fn fdopen
fn bind
fn clonefileat
fn task_map_kcdata_object_64
fn flockfile
fn setrlimit
fn wcpncpy
fn mlock
fn __sputc
fn psignal
fn thread_set_exception_ports
fn strtol
fn mbsrtowcs
fn host_create_mach_voucher_trap
fn getservbyname
fn fchownat
fn wcstoul
fn wmemset
fn mlockall
fn imaxabs
fn pthread_key_delete
fn strtoll
fn mach_vm_region_info
fn getsockopt
fn host_set_exception_ports
fn popen
fn setenv
fn vm_purgable_control
fn poll
fn host_swap_exception_ports
fn task_create
fn wctrans
fn mig_allocate
fn getrusage
fn task_get_emulation_vector
fn strspn
fn iswphonogram
fn srand48
fn __darwin_fd_isset
fn task_get_mach_voucher
fn thread_abort
fn mach_task_is_self
fn _kernelrpc_mach_port_insert_member_trap
fn vm_copy
fn host_info
fn setpgrp
fn processor_set_policy_disable
fn execve
fn memccpy
fn posix_spawnp
fn semaphore_signal_thread
fn vm_read_overwrite
fn host_set_atm_diagnostic_flag
fn gethostent
fn host_request_notification
fn ___toupper
fn posix_spawnattr_setsigmask
fn uname
fn wcsspn
fn telldir
fn thread_assign
fn atomic_flag_clear
fn fchmod
fn mkfifoat
fn mach_port_move_member
fn mach_port_kobject
fn thread_suspend
fn vm_region_recurse_64
fn pthread_getconcurrency
fn voucher_mach_msg_clear
fn fchmodat
fn tzset
fn toascii
fn wmemcmp
fn shm_unlink
fn stat
fn times
fn thread_wire
fn task_info
fn thread_get_state
fn tmpfile
fn endprotoent
fn mprotect
fn mach_port_insert_right
fn task_set_port_space
fn setbuf
fn mach_thread_self
fn fsetpos
fn processor_start
fn macx_backing_store_suspend
fn _kernelrpc_mach_vm_allocate_trap
fn clock
fn _dyld_image_count
fn atoll
fn socket
fn sem_post
fn ftruncate
fn flistxattr
fn iswlower
fn recv
fn funlockfile
fn ftrylockfile
fn dirfd
fn sigaddset
fn vm_mapped_pages_info
fn ___runetype
fn _dyld_all_twolevel_modules_prebound
fn atol
fn fclose
fn sendmsg
fn if_freenameindex
fn isprint
fn strncmp
fn unlink
fn atomic_flag_clear_explicit
fn localtime_r
fn towctrans
fn sigwait
fn fchown
fn _OSWriteSwapInt16
fn exit
fn strndup
fn lcong48
fn srand
fn chmod
fn thread_get_exception_ports_info
fn macx_swapoff
fn mach_msg_destroy
fn feclearexcept
fn feof
fn atomic_thread_fence
fn iswnumber
fn kill
fn setstate
fn iswascii
fn setuid
fn mach_make_memory_entry_64
fn putchar
fn mach_port_set_seqno
fn mach_port_destruct
fn _OSReadInt16
fn strnlen
fn tolower
fn sem_destroy
fn vm_map_page_query
fn host_page_size
fn linkat
fn task_register_dyld_get_process_state
fn renameat
fn iconv_open
fn closelog
fn processor_assign
fn iswspace
fn mach_port_type
fn thread_create
fn _tlv_bootstrap
fn host_get_atm_diagnostic_flag
fn wmemmove
fn NXSwapHostLongToBig
fn _kernelrpc_mach_vm_deallocate_trap
fn NXHostByteOrder
fn sem_getvalue
fn _kernelrpc_mach_port_unguard_trap
fn mmap
fn __toupper
fn nrand48
fn lldiv
fn posix_spawn_file_actions_addclose
fn thread_get_assignment
fn _OSWriteSwapInt32
fn seed48
fn _OSReadInt64
fn task_get_special_port
fn wcsncmp
fn setgroupent
fn if_nametoindex
fn duplocale
fn host_get_io_main
fn posix_spawn_file_actions_adddup2
fn wcsnrtombs
fn msgget
fn task_register_hardened_exception_handler
fn mach_port_construct
fn host_processor_info
fn getxattr
fn _dyld_get_image_name
fn unsetenv
fn fwide
fn getservent
fn NSLinkEditError
fn host_set_multiuser_config_flags
fn fsetxattr
fn alphasort
fn usleep
fn processor_set_tasks
fn task_resume
fn aio_fsync
fn task_for_pid
fn NXSwapHostIntToBig
fn getpwent
fn NSLinkModule
fn NSIsSymbolNameDefined
fn recvmsg
fn localeconv
fn vdprintf
fn getnetbyname
fn NSAddImage
fn kevent64
fn thread_resume
fn add
  bb0 bb0
    alloca Virtual { id: 0, bank: General, size_bits: 64 }, 1
    add Virtual { id: 1, bank: General, size_bits: 64 }, symbol(local.1), symbol(local.2)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 1, bank: General, size_bits: 64 }
    load Virtual { id: 3, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 0, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    ret
fn main
  bb0 bb0
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    intrinsic.call symbol(intrinsic.println)
    call symbol(summarize)(struct(len=2), 3, true) cc=C tail=false
    alloca Virtual { id: 9, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 9, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 8, bank: General, size_bits: 64 }
    br
  bb1 bb1
    bitcast Virtual { id: 11, bank: General, size_bits: 64 }, Virtual { id: 9, bank: General, size_bits: 64 }
    load Virtual { id: 12, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 11, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 12, bank: General, size_bits: 64 }
    call symbol(summarize)(struct(len=2), 7, false) cc=C tail=false
    alloca Virtual { id: 15, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 15, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 14, bank: General, size_bits: 64 }
    br
  bb2 bb2
    bitcast Virtual { id: 17, bank: General, size_bits: 64 }, Virtual { id: 15, bank: General, size_bits: 64 }
    load Virtual { id: 18, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 17, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.println), Virtual { id: 18, bank: General, size_bits: 64 }
    call symbol(add)(5, 2) cc=C tail=false
    br
  bb3 bb3
    intrinsic.call symbol(intrinsic.println), Virtual { id: 20, bank: General, size_bits: 64 }
    ret
fn summarize
  bb0 bb0
    alloca Virtual { id: 22, bank: General, size_bits: 64 }, 1
    alloca Virtual { id: 23, bank: General, size_bits: 64 }, 1
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 23, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), symbol(local.1)
    bitcast Virtual { id: 25, bank: General, size_bits: 64 }, Virtual { id: 23, bank: General, size_bits: 64 }
    load Virtual { id: 26, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 25, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(8), address_space: None, pre_indexed: false, post_indexed: false })
    intrinsic.call symbol(intrinsic.format), Virtual { id: 26, bank: General, size_bits: 64 }, symbol(local.2), symbol(local.3)
    store mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: None, address_space: None, pre_indexed: false, post_indexed: false }), Virtual { id: 27, bank: General, size_bits: 64 }
    load Virtual { id: 29, bank: General, size_bits: 64 }, mem(AsmMemoryOperand { base: Some(Virtual { id: 22, bank: General, size_bits: 64 }), index: None, scale: 1, displacement: 0, segment: None, size_bytes: Some(16), address_space: None, pre_indexed: false, post_indexed: false })
    ret


Symbols:
  add                              0x00000000
  main                             0x00000060
  summarize                        0x00000264

Text relocations:
  offset=0x00000070 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000007c kind=CallRel32 symbol=printf addend=0
  offset=0x00000080 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000008c kind=CallRel32 symbol=printf addend=0
  offset=0x00000090 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000009c kind=CallRel32 symbol=printf addend=0
  offset=0x000000a0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x000000ac kind=CallRel32 symbol=printf addend=0
  offset=0x000000c0 kind=Aarch64AdrpAdd symbol=__const_data_0 addend=0
  offset=0x0000014c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000164 kind=CallRel32 symbol=printf addend=0
  offset=0x00000178 kind=Aarch64AdrpAdd symbol=__const_data_1 addend=0
  offset=0x00000204 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000021c kind=CallRel32 symbol=printf addend=0
  offset=0x00000234 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000024c kind=CallRel32 symbol=printf addend=0
  offset=0x000002f0 kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x00000320 kind=CallRel32 symbol=snprintf addend=0
  offset=0x00000338 kind=CallRel32 symbol=malloc addend=0
  offset=0x0000034c kind=Aarch64AdrpAdd symbol=fp_rodata_base addend=0
  offset=0x0000037c kind=CallRel32 symbol=snprintf addend=0

.text (1028 bytes):
  00000000  ff 43 01 d1 fd 7b 04 a9  fd 03 00 91 e0 17 00 f9 
  00000010  e1 1b 00 f9 1f 20 03 d5  f0 03 00 91 10 e2 00 91 
  00000020  f0 03 00 f9 f0 17 40 f9  f1 1b 40 f9 10 02 11 8b 
  00000030  f0 07 00 f9 f1 03 40 f9  f0 07 40 f9 30 02 00 f9 
  00000040  f0 03 40 f9 11 02 40 f9  f1 0f 00 f9 e0 0f 40 f9 
  00000050  bf 03 00 91 fd 7b 44 a9  ff 43 01 91 c0 03 5f d6 
  00000060  ff c3 05 d1 fd 7b 16 a9  fd 03 00 91 1f 20 03 d5 
  00000070  00 00 00 90 00 00 00 91  00 40 00 91 00 00 00 94 
  00000080  00 00 00 90 00 00 00 91  00 c0 00 91 00 00 00 94 
  00000090  00 00 00 90 00 00 00 91  00 80 01 91 00 00 00 94 
  000000a0  00 00 00 90 00 00 00 91  00 60 02 91 00 00 00 94 
  000000b0  e0 03 00 91 00 60 04 91  f1 03 00 91 31 22 04 91 
  000000c0  10 00 00 90 10 02 00 91  e9 03 11 aa 30 01 00 f9 
  000000d0  b0 00 80 d2 10 00 a0 f2  10 00 c0 f2 10 00 e0 f2 
  000000e0  e9 03 11 aa 29 21 00 91  30 01 00 f9 e1 03 11 aa 
  000000f0  62 00 80 d2 23 00 80 d2  5b 00 00 94 f0 03 00 91 
  00000100  10 62 04 91 f0 1b 00 f9  f0 03 00 91 10 e2 04 91 
  00000110  f0 1f 00 f9 f1 1f 40 f9  f0 8f 40 f9 e9 03 11 aa 
  00000120  30 01 00 f9 f0 93 40 f9  e9 03 11 aa 29 21 00 91 
  00000130  30 01 00 f9 01 00 00 14  f0 1f 40 f9 f0 27 00 f9 
  00000140  f0 27 40 f9 11 02 40 f9  f1 2b 00 f9 00 00 00 90 
  00000150  00 00 00 91 00 80 02 91  e1 2b 40 f9 f0 2b 40 f9 
  00000160  f0 03 00 f9 00 00 00 94  e0 03 00 91 00 a0 04 91 
  00000170  f1 03 00 91 31 22 04 91  10 00 00 90 10 02 00 91 
  00000180  e9 03 11 aa 30 01 00 f9  90 00 80 d2 10 00 a0 f2 
  00000190  10 00 c0 f2 10 00 e0 f2  e9 03 11 aa 29 21 00 91 
  000001a0  30 01 00 f9 e1 03 11 aa  e2 00 80 d2 03 00 80 d2 
  000001b0  2d 00 00 94 f0 03 00 91  10 a2 04 91 f0 33 00 f9 
  000001c0  f0 03 00 91 10 22 05 91  f0 37 00 f9 f1 37 40 f9 
  000001d0  f0 97 40 f9 e9 03 11 aa  30 01 00 f9 f0 9b 40 f9 
  000001e0  e9 03 11 aa 29 21 00 91  30 01 00 f9 01 00 00 14 
  000001f0  f0 37 40 f9 f0 3f 00 f9  f0 3f 40 f9 11 02 40 f9 
  00000200  f1 43 00 f9 00 00 00 90  00 00 00 91 00 c0 02 91 
  00000210  e1 43 40 f9 f0 43 40 f9  f0 03 00 f9 00 00 00 94 
  00000220  a0 00 80 d2 41 00 80 d2  76 ff ff 97 e0 4b 00 f9 
  00000230  01 00 00 14 00 00 00 90  00 00 00 91 00 00 03 91 
  00000240  e1 4b 40 f9 f0 4b 40 f9  f0 03 00 f9 00 00 00 94 
  00000250  bf 03 00 91 fd 7b 56 a9  ff c3 05 91 00 00 80 d2 
  00000260  c0 03 5f d6 ff c3 03 d1  fd 7b 0e a9 fd 03 00 91 
  00000270  e0 4b 00 f9 e9 03 01 aa  30 01 40 f9 f0 3b 00 f9 
  00000280  e9 03 01 aa 29 21 00 91  30 01 40 f9 f0 3f 00 f9 
  00000290  e2 43 00 f9 e3 23 02 39  1f 20 03 d5 f0 03 00 91 
  000002a0  10 e2 02 91 f0 13 00 f9  f0 03 00 91 10 22 03 91 
  000002b0  f0 17 00 f9 f1 17 40 f9  f0 3b 40 f9 e9 03 11 aa 
  000002c0  30 01 00 f9 f0 3f 40 f9  e9 03 11 aa 29 21 00 91 
  000002d0  30 01 00 f9 f0 17 40 f9  f0 1f 00 f9 f0 1f 40 f9 
  000002e0  11 02 40 f9 f1 23 00 f9  00 00 80 d2 01 00 80 d2 
  000002f0  02 00 00 90 42 00 00 91  42 40 03 91 e3 23 40 f9 
  00000300  f0 23 40 f9 f0 03 00 f9  e4 43 40 f9 f0 43 40 f9 
  00000310  f0 07 00 f9 e5 23 42 39  f0 23 42 39 f0 0b 00 f9 
  00000320  00 00 00 94 f0 03 00 aa  f0 53 00 f9 10 06 00 91 
  00000330  f0 27 00 f9 e0 03 10 aa  00 00 00 94 e9 03 00 aa 
  00000340  e0 03 09 aa e1 27 40 f9  e9 27 00 f9 02 00 00 90 
  00000350  42 00 00 91 42 40 03 91  e3 23 40 f9 f0 23 40 f9 
  00000360  f0 03 00 f9 e4 43 40 f9  f0 43 40 f9 f0 07 00 f9 
  00000370  e5 23 42 39 f0 23 42 39  f0 0b 00 f9 00 00 00 94 
  00000380  e9 27 40 f9 e9 4f 00 f9  f1 13 40 f9 f0 4f 40 f9 
  00000390  e9 03 11 aa 30 01 00 f9  f0 53 40 f9 e9 03 11 aa 
  000003a0  29 21 00 91 30 01 00 f9  f1 13 40 f9 e9 03 11 aa 
  000003b0  30 01 40 f9 f0 57 00 f9  e9 03 11 aa 29 21 00 91 
  000003c0  30 01 40 f9 f0 5b 00 f9  f0 03 00 91 10 a2 02 91 
  000003d0  f0 2f 00 f9 f1 4b 40 f9  f0 57 40 f9 e9 03 11 aa 
  000003e0  30 01 00 f9 f0 5b 40 f9  e9 03 11 aa 29 21 00 91 
  000003f0  30 01 00 f9 bf 03 00 91  fd 7b 4e a9 ff c3 03 91 
  00000400  c0 03 5f d6 

.rodata (238 bytes):
  00000000  61 6c 70 68 61 00 62 65  74 61 00 00 00 00 00 00 
  00000010  54 75 74 6f 72 69 61 6c  3a 20 33 33 5f 6e 61 6d 
  00000020  65 64 5f 61 72 67 73 2e  66 70 0a 00 00 00 00 00 
  00000030  46 6f 63 75 73 3a 20 4e  61 6d 65 64 20 61 72 67 
  00000040  75 6d 65 6e 74 73 20 69  6e 20 66 75 6e 63 74 69 
  00000050  6f 6e 20 63 61 6c 6c 73  0a 00 00 00 00 00 00 00 
  00000060  45 78 70 65 63 74 61 74  69 6f 6e 3a 20 6b 65 79 
  00000070  77 6f 72 64 20 61 72 67  75 6d 65 6e 74 73 20 63 
  00000080  61 6e 20 62 65 20 72 65  6f 72 64 65 72 65 64 0a 
  00000090  00 00 00 00 00 00 00 00  0a 00 00 00 00 00 00 00 
  000000a0  66 69 72 73 74 3a 20 25  73 0a 00 00 00 00 00 00 
  000000b0  73 65 63 6f 6e 64 3a 20  25 73 0a 00 00 00 00 00 
  000000c0  61 64 64 3a 20 25 6c 6c  64 0a 00 00 00 00 00 00 
  000000d0  6c 61 62 65 6c 3d 25 73  20 63 6f 75 6e 74 3d 25 
  000000e0  6c 6c 64 20 61 63 74 69  76 65 3d 25 64 00 
