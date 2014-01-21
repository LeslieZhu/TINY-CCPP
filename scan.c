/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the TINY compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"

/* states in scanner DFA */
typedef enum { 
    START,INASSIGN,INCOMMENT,INNUM,INID,DONE 
} StateType;

/* lexeme of identifier or reserved word */
char tokenString[MAXTOKENLEN+1];
char *allocp = tokenString;


/* BUFLEN = length of the input buffer for
   source code lines */
#define BUFLEN 256

static char lineBuf[BUFLEN]; /* holds the current line */
static int linepos = 0; /* current position in LineBuf */
static int bufsize = 0; /* current size of buffer string */
static int EOF_flag = FALSE; /* corrects ungetNextChar behavior on EOF */

/* getNextChar fetches the next non-blank character
   from lineBuf, reading in a new line if lineBuf is
   exhausted */
static int getNextChar(void)
{ if (!(linepos < bufsize))
  { lineno++;
    if (fgets(lineBuf,BUFLEN-1,source))
    { if (EchoSource) fprintf(listing,"%4d: %s",lineno,lineBuf);
      bufsize = strlen(lineBuf);
      linepos = 0;
      return lineBuf[linepos++];
    }
    else
    { EOF_flag = TRUE;
      return EOF;
    }
  }
  else return lineBuf[linepos++];
}

/* ungetNextChar backtracks one character
   in lineBuf */
static void ungetNextChar(void)
{ if (!EOF_flag) linepos-- ;}

/* lookup table of reserved words */
/* for bianysearch function */
static struct
    { char* str;
      TokenType tok;
    } reservedWords[MAXRESERVED] = {
    {"else",ELSE},
    {"end",END},
    {"if",IF},
    {"read",READ},
    {"repeat",REPEAT},
    {"then",THEN},
    {"until",UNTIL},
    {"write",WRITE}};


/* binary search */
/* usage: binarysearch(0,MAXRESERVED,s) */
static TokenType binarysearch(int start, int end, char * s)
{
    int mid =  (start+end)%2 == 0 ? (start+end)/2 : (start+end)/2+1;
    //int debug=0;

    printf("debug: start=%d, mid=%d, end=%d, str=%s,find-str=%s\n",start,mid,end,s,reservedWords[mid].str);
    //scanf("%c",&debug);

    if(mid == end){
        printf("debug, find it! str=%s, return ID\n",s);
        return ID;
    }
    
    if (strcmp(reservedWords[mid].str, s) > 0){ 
        return binarysearch(start,mid,s);
    } else if (strcmp(reservedWords[mid].str, s) < 0){ 
        return binarysearch(mid,end,s);
    } else {
        printf("debug, find it! str=%s, find-str=%s\n",s,reservedWords[mid].str);
        return reservedWords[mid].tok;
    }
}



/* lookup an identifier to see if it is a reserved word */
/* uses linear search */
/* 线性查找 */
static TokenType reservedLookup (char * s)
{ 
    /* debug begin*/
    return binarysearch(0,MAXRESERVED-1,s); 

    int i;
    for (i=0;i<MAXRESERVED;i++)
        {
            if (!strcmp(s,reservedWords[i].str))
                {
                    printf("debug, find it! str=%s, find-str=%s\n",s,reservedWords[i].str);
                    return reservedWords[i].tok;
                }
        }
    printf("debug, find it! str=%s, find-str=ID\n",s);
    return ID;
}

/****************************************/
/* the primary function of the scanner  */
/****************************************/
/* function getToken returns the 
 * next token in source file
 */
TokenType getToken(void)
{  /* index for storing into tokenString */
   int tokenStringIndex = 0;
   /* holds current token to be returned */
   TokenType currentToken;
   /* current state - always begins at START */
   StateType state = START;
   /* flag to indicate save to tokenString */
   int save;
   while (state != DONE)
   { int c = getNextChar();
     save = TRUE;
     switch (state)
     { case START:
         if (isdigit(c))
           state = INNUM;
         else if (isalpha(c))
           state = INID;
         else if (c == ':')
           state = INASSIGN;
         else if ((c == ' ') || (c == '\t') || (c == '\n'))
           save = FALSE;
         else if (c == '{')
         { save = FALSE;
           state = INCOMMENT;
         }
         else
         { state = DONE;
           switch (c)
           { case EOF:
               save = FALSE;
               currentToken = ENDFILE;
               break;
             case '=':
               currentToken = EQ;
               break;
             case '<':
               currentToken = LT;
               break;
             case '+':
               currentToken = PLUS;
               break;
             case '-':
               currentToken = MINUS;
               break;
             case '*':
               currentToken = TIMES;
               break;
             case '/':
               currentToken = OVER;
               break;
             case '(':
               currentToken = LPAREN;
               break;
             case ')':
               currentToken = RPAREN;
               break;
             case ';':
               currentToken = SEMI;
               break;
             default:
               currentToken = ERROR;
               break;
           }
         }
         break;
       case INCOMMENT:
         save = FALSE;
         if (c == EOF)
         { state = DONE;
           currentToken = ENDFILE;
         }
         else if (c == '}') state = START;
         break;
       case INASSIGN:
         state = DONE;
         if (c == '=')
           currentToken = ASSIGN;
         else
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           currentToken = ERROR;
         }
         break;
       case INNUM:
         if (!isdigit(c))
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           state = DONE;
           currentToken = NUM;
         }
         break;
       case INID:
         if (!isalpha(c))
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           state = DONE;
           currentToken = ID;
         }
         break;
       case DONE:
       default: /* should never happen */
         fprintf(listing,"Scanner Bug: state= %d\n",state);
         state = DONE;
         currentToken = ERROR;
         break;
     }

     if (save){
         if(tokenStringIndex <= MAXTOKENLEN){ /* 2.29 */
             allocp[tokenStringIndex++] = (char) c;
         } else if(tokenStringIndex <= ALLOCSIZE){ /* still has space */
             if(strlen(allocp) == (MAXTOKENLEN - 1)){
                 allocp = (char *)malloc(sizeof(char)*ALLOCSIZE);
                 strcpy(allocp,tokenString);
             }
             allocp[tokenStringIndex++] = (char) c;
         } 
     }

     if (state == DONE)
     { allocp[tokenStringIndex] = '\0';
       if (currentToken == ID)
         currentToken = reservedLookup(allocp);
     }
   }
   if (TraceScan) {
     fprintf(listing,"\t%d: ",lineno);
     printToken(currentToken,allocp);
   }

   /* free */
   if (allocp >= tokenString && strlen(allocp) < ALLOCSIZE){
       allocp = tokenString;
   }

   return currentToken;
} /* end getToken */

