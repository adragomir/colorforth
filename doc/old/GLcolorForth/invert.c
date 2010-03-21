/* invert font data */
#include <stdio.h>
int main () {
    unsigned short s[24]; int i;
    while (fread (s, 2, 24, stdin))
	for (i = 0; i < 24; i++) fwrite(&s[23-i], 2, 1,stdout);
    return 0;
}
