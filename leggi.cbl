 IDENTIFICATION DIVISION.
 PROGRAM-ID. LEGGI.
 AUTHOR. LORELLA
 DATE-WRITTEN. 13/3/2017
* QUESTO E' UN COMMENTO
*
 ENVIRONMENT DIVISION.  
* 
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.   
* INPUT FILE 

     SELECT PRIMOF   ASSIGN TO 'TERZ.TXT'
     ORGANIZATION IS SEQUENTIAL
     ACCESS MODE IS SEQUENTIAL.
* OUTPUT FILE 

     SELECT OUTF ASSIGN TO 'SOUTF2.TXT'
     ORGANIZATION IS SEQUENTIAL
     ACCESS MODE IS SEQUENTIAL.
*
  
************************************************************
*                       DATA DIVISION                       
************************************************************
 DATA DIVISION.
 FILE SECTION.
 FD   PRIMOF
*      LABEL RECORD IS OMITTED
      DATA RECORD IS REC-PRIMOF.
 01 REC-PRIMOF  PIC X(10).
 FD   OUTF
*      LABEL RECORD IS OMITTED
      DATA RECORD IS REC-OUTF.
 01 REC-OUTF  PIC X(10).
************************************************************
*                WORKING-STORAGE SECTION                     
************************************************************

 WORKING-STORAGE SECTION.   
 01   WS-PRIMOF.
      05 NOME           PIC X(5).
      05 COGNOME        PIC X(5). 
      
 01   WS-OUTF.     
      05 OUT-COGNOME    PIC X(5). 
      05 OUT-NOME       PIC X(5).
*                
    
 01 FINE-FILE PIC X VALUE SPACES.
************************************************************
*                PROCEDURE DIVISION                    
************************************************************
 PROCEDURE DIVISION.    
      
 MAIN-LINE.
      PERFORM APERTURA THRU FINE-APERTURA.
      PERFORM ELABORA THRU FINE-ELABORA UNTIL FINE-FILE = 'S'.
      PERFORM CHIUSURA THRU FINE-CHIUSURA.
      
      STOP RUN.
 
 APERTURA.  
 
     OPEN INPUT PRIMOF.
     OPEN OUTPUT OUTF.   
     READ PRIMOF INTO WS-PRIMOF AT END 
          MOVE 'S' TO FINE-FILE
          DISPLAY 'FILE PRIMOF VUOTO'. 
          
 FINE-APERTURA.   
      EXIT.
* 
 ELABORA.    
    
     MOVE WS-PRIMOF TO WS-OUTF.
     
     
     DISPLAY 'WS PRIMOF'.
     DISPLAY WS-PRIMOF.         
     
     WRITE REC-OUTF FROM WS-OUTF.
     
      READ PRIMOF INTO WS-PRIMOF AT END MOVE 'S' TO FINE-FILE.   
      
 FINE-ELABORA.    
      EXIT.
* 
 CHIUSURA. 
 
     CLOSE PRIMOF
           OUTF. 
           
 FINE-CHIUSURA.    
      EXIT.