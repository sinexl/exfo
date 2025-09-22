#include <stdio.h>
void print_i64(long long num) {
    printf("%lld\n", num);
}

void print_f64(double num) { 
    printf("%f\n", num); 
}

void print_bool(bool b) { 
    printf("%s\n", b ? "true" : "false");
}