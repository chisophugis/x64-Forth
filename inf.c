/* Trigger an infinite loop by writing machine code directly into
 * executable pages.
 */

#define _GNU_SOURCE /* MAP_ANONYMOUS */

#include <string.h> /* memcpy */
#include <stdint.h> /* uint8_t */
#include <assert.h>

#include <sys/mman.h> /* mmap */

/* CALL to an arbitrary address */
typedef void(*void_fn)(void);
#define CALL(adr) (*(void_fn)(adr))()

/* If executed on an x86 processor, this will trigger an infinite loop */
/* Note: a real-world spin loop should include a PAUSE instruction */
uint8_t jmp_code[] = {
	[0] = 0xEB, /* JMP rel8 */
	[1] = 0xFE /* -2 (bytes) == put us back on this instruction */
};

int main(int argc, char *argv[])
{
	void *page = mmap(
		NULL, 4096,
		PROT_EXEC|PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_ANONYMOUS,
		-1, 0 /* ignored with MAP_ANONYMOUS */
	);

	/* move in the code */
	memcpy(page, jmp_code, sizeof(jmp_code));
	/* descend into the infinite loop */
	CALL(page);

	assert(0 && "Should not be reached");
}
