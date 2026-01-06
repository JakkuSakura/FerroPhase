// C++ exception runtime for catch_unwind/panic in the LLVM backend.
#include <exception>
#include <stdio.h>

struct FpPanic {
    const char* message;
};

extern "C" void fp_panic(const char* message) {
    const char* msg = message ? message : "panic";
    fprintf(stderr, "panic: %s\n", msg);
    throw FpPanic{msg};
}
