 IDENTIFICATION DIVISION.
 PROGRAM-ID. TABORD.
 AUTHOR. SIMONE
 DATE-WRITTEN. 18/10/2022
* QUESTO E' UN COMMENTO
*
 ENVIRONMENT DIVISION.  
* 
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.   
* INPUT FILE 

     SELECT PRIMOF   ASSIGN TO 'ANAGR.TXT'
     ORGANIZATION IS SEQUENTIAL
     ACCESS MODE IS SEQUENTIAL.
* OUTPUT FILE 


*
                                                            
*                       DATA DIVISION                       
 DATA DIVISION.
 FILE SECTION.
 FD   PRIMOF
*      LABEL RECORD IS OMITTED
      DATA RECORD IS REC-PRIMOF.
 01 REC-PRIMOF  PIC X(33).
                                                            
*                WORKING-STORAGE SECTION                    

 WORKING-STORAGE SECTION.   
 01   WS-PRIMOF.
      05 NOME           PIC X(10).
      05 COGNOME        PIC X(10). 
      05 ANNI           PIC 999.
      
 01   WS-OUTF.     
      05 OUT-NOME       PIC X(10). 
      05 OUT-COGNOME    PIC X(10).
      05 OUT-ANNI       PIC 999.
      
 01   TABELLA.
      05 PERSONA OCCURS 6 TIMES.
         10 TAB-NOME    PIC X(10).
         10 TAB-COGNOME PIC X(10).
         10 TAB-ANNI    PIC 999.
 01   CONT              PIC 99 VALUE 0.
*                
    
 01 FINE-FILE PIC X VALUE SPACES.                           
*                PROCEDURE DIVISION                        
 PROCEDURE DIVISION.    
      
 MAIN-LINE.
      PERFORM APERTURA THRU FINE-APERTURA.
      PERFORM ELABORA  THRU FINE-ELABORA UNTIL FINE-FILE = 'S'.
      PERFORM CHIUSURA THRU FINE-CHIUSURA.
      
      STOP RUN.
 
 APERTURA.  
 
     OPEN INPUT PRIMOF.
     
     READ PRIMOF INTO WS-PRIMOF AT END 
          MOVE 'S' TO FINE-FILE
          DISPLAY 'FILE PRIMOF VUOTO'. 
          
 FINE-APERTURA.   
      EXIT.
* 
 ELABORA.    
    
     MOVE WS-PRIMOF TO WS-OUTF.
     
     ADD 1 TO CONT.
     
     MOVE WS-OUTF TO PERSONA(CONT).
     DISPLAY 'RIGA ' CONT ' ' PERSONA(CONT).
     
     
     READ PRIMOF INTO WS-PRIMOF AT END MOVE 'S' TO FINE-FILE.   
      
 FINE-ELABORA.
      EXIT.
* 
 CHIUSURA.
 
     CLOSE PRIMOF.

                                
 FINE-CHIUSURA.    
     
     
     
     EXIT.