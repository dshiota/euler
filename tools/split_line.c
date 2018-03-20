#include <stdio.h>
#include <stdint.h>

int32_t main(void)
{
	char *file="p042_words.txt";
	char c;
	int32_t cnt = 0;
	FILE *fp = fopen(file, "r");

	while ( (c = fgetc(fp) ) != EOF) {
		fputc(c, stdout);
		if (c == (int)',') {
			cnt += 1;
		}
		if (cnt == 7) {
			fputc('\n', stdout);
			cnt = 0;
		}
	}

	fclose(fp);
		
	return 0;
}
