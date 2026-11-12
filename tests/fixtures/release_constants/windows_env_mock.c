// SPDX-License-Identifier: MPL-2.0
#include <stdint.h>
#include <string.h>
static int scenario, acquired, released, converted, failed;
static uint16_t block[] = {'=', 'C', ':', '=', 'x', 0, 0xd55c, '=', 0xd83d, 0xde00, 0, 'E', '=', 0, 0};
static const unsigned char bytes[] = {'=', 'C', ':', '=', 'x', 0, 0xed, 0x95, 0x9c, '=', 0xf0, 0x9f, 0x98, 0x80, 0, 'E', '=', 0, 0};
void choose(int n) { scenario = n; acquired = released = converted = 0; }
int failures(void) { return failed; }
int acquisitions(void) { return acquired; }
int releases(void) { return released; }
int conversions(void) { return converted; }
uint16_t *GetEnvironmentStringsW(void) { acquired++; return scenario == 1 ? 0 : block; }
int FreeEnvironmentStringsW(uint16_t *p) { if (p != block) failed++; released++; return scenario != 5; }
uint32_t GetLastError(void) { return 5; }
int WideCharToMultiByte(uint32_t cp, uint32_t flags, const uint16_t *src, int units,
                       unsigned char *dst, int cap, const char *def, int *used) {
    if (cp != 65001 || flags != 128 || src != block || units != 15 || def || used) failed++;
    if (!dst) { if (cap) failed++; return scenario == 2 ? 0 : sizeof bytes; }
    converted++;
    if (cap != sizeof bytes) failed++;
    if (scenario == 3) return 0;
    memcpy(dst, bytes, sizeof bytes); return sizeof bytes;
}


// ABI fixture only: native Unicode folding is tested against the Windows API.
int MultiByteToWideChar(uint32_t cp, uint32_t flags, const unsigned char *src, int bytes,
                        uint16_t *dst, int cap) {
    if (cp != 65001 || flags != 8 || !src || bytes <= 0) { failed++; return 0; }
    if (scenario == 6) { return 0; }
    int units = 0;
    for (int i = 0; i < bytes; i++) {
        uint32_t c = src[i];
        if (c >= 128) {
            if ((c & 224) != 192 || i + 1 >= bytes || (src[i + 1] & 192) != 128) { return 0; }
            c = ((c & 31) << 6) | (src[++i] & 63);
        }
        if (dst) {
            if (units >= cap) { failed++; return 0; }
            dst[units] = c;
        } else if (cap) { failed++; return 0; }
        units++;
    }
    return units;
}

int CompareStringOrdinal(const uint16_t *a, int na, const uint16_t *b, int nb, int ignore_case) {
    if (!a || !b || na <= 0 || nb <= 0 || ignore_case != 1) { failed++; return 0; }
    if (scenario == 7) { return 0; }
    if (na != nb) { return 1; }
    for (int i = 0; i < na; i++) {
        uint16_t left = a[i], right = b[i];
        if (left >= 'a' && left <= 'z') { left -= 32; }
        if (right >= 'a' && right <= 'z') { right -= 32; }
        if (left != right) { return 1; }
    }
    return 2;
}
