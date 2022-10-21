 IDENTIFICATION DIVISION.
 PROGRAM-ID. INDICI.
 AUTHOR. LORELLA
 DATE-WRITTEN. 21/10/2022
* QUESTO E' UN COMMENTO
*
 ENVIRONMENT DIVISION.  
* 
 SOURCE-COMPUTER. IBM-PC.
 OBJECT-COMPUTER. IBM-PC.
 INPUT-OUTPUT SECTION.
 FILE-CONTROL.   
* INPUT FILE 

     SELECT PRIMOF   ASSIGN TO 'FILIALI.IDX'
     ORGANIZATION IS INDEXED
     ACCESS MODE IS DYNAMIC
     RECORD KEY COD-FILIALE
     STATUS WS-STATUS.
* OUTPUT FILE 

     
*
  
************************************************************
*                       DATA DIVISION                       
************************************************************
 DATA DIVISION.
 FILE SECTION.
 FD   PRIMOF
*      LABEL RECORD IS OMITTED
      DATA RECORD IS REC-PRIMOF.
 01 REC-PRIMOF.
    05 COD-FILIALE   PIC 9(4).
    05 DESC-FILIALE  PIC X(16). 

************************************************************
*                WORKING-STORAGE SECTION                     
************************************************************

 WORKING-STORAGE SECTION.   
 01   WS-PRIMOF.
      05 WS-COD-FILIALE   PIC 9(4).
      05 WS-DESC-FILIALE  PIC X(16).
      
 
 01 WS-STATUS PIC XX.   
 01 FINE-FILE PIC X VALUE SPACES.
************************************************************
*                PROCEDURE DIVISION                    
************************************************************
 PROCEDURE DIVISION.    
      
 MAIN-LINE.
      PERFORM APERTURA THRU FINE-APERTURA.
*      PERFORM ELABORA THRU FINE-ELABORA UNTIL FINE-FILE = 'S'.
      PERFORM CHIUSURA THRU FINE-CHIUSURA.
      
      STOP RUN.
 
 APERTURA.  
 
     OPEN I-O PRIMOF.
     MOVE 1001   TO COD-FILIALE.
     READ PRIMOF INTO WS-PRIMOF INVALID KEY
          
          DISPLAY 'ERRORE'
          DISPLAY WS-STATUS 
     NOT INVALID KEY     
           DISPLAY 'RECORD LETTO'     
           DISPLAY WS-PRIMOF. 
     MOVE 9999   TO COD-FILIALE.
     READ PRIMOF INTO WS-PRIMOF INVALID KEY
          
          DISPLAY 'ERRORE'
          DISPLAY WS-STATUS
     NOT INVALID KEY     
            DISPLAY 'RECORD LETTO'     
            DISPLAY WS-PRIMOF.
          
 FINE-APERTURA.   
      EXIT.
* 

* 
 CHIUSURA. 
 
     CLOSE PRIMOF.
            
           
 FINE-CHIUSURA.    
      EXIT.