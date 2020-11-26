#include <sys/wait.h>
#include <sys/types.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
    int status;
    int ret = waitpid(0, &status, WUNTRACED);
    if (ret == -1) {
        perror("waitpid");
        exit(1);
    }
    printf("No error");
}
    


