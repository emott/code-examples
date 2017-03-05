#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sysexits.h>

#define MAX_LEN 1023

int main(){
  char str[MAX_LEN + 1];
  char* argv[3];
  
  while(1){
    argv[0] = malloc(MAX_LEN + 1); /* malloc space for the command arguments. */
    argv[1] = malloc(MAX_LEN + 1);
    argv[2] = NULL;
    argv[0][0] = '\0';
    argv[1][0] = '\0';
    
    printf("shell_jr: ");
    fflush(stdout);

    if(fgets(str, sizeof(str), stdin) == NULL){
      break;
    }
    
    if(sscanf(str, "%s %s", argv[0], argv[1]) == EOF){
      break;
    }

    if(strcmp(argv[1], "\0") == 0){ /* there was not a second command, */
      argv[1] = NULL;		/*  so argv[1] was not changed. Now we make it
				    NULL so execvp will stop */
    }
   
    if(strcmp(argv[0], "exit") == 0 || strcmp(argv[0], "hastalavista") == 0){
      printf("See you\n");
      exit(0);
    }else if(strcmp(argv[0], "cd") == 0){
      if(chdir(argv[1]) == -1){
	     printf("Cannot change to directory %s\n", argv[1]);
	 fflush(stdout);
      }
    }else{
      
      switch(fork()) {
	
      case 0: /*inside child */
	if(execvp(argv[0], (char * const *)argv) == -1){ /*executing the command */
	  printf("Failed to execute %s\n", argv[0]);
	  fflush(stdout);
	  exit(EX_OSERR);
	}
	break;
      case -1:
	err(EX_OSERR, "fork error");
	break;
      default: /* inside parent */
	wait(NULL);
	break;
      } /* end switch */
      
    }/* end else */

    free(argv[0]);
    free(argv[1]);
    
   
  } /* end while */

  return 0;
}
