 IDENTIFICATION DIVISION.
 PROGRAM-ID. CRUD.
 AUTHOR. LUCIO
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

*     SELECT PRIMOF   ASSIGN TO 'FILIALI.IDX'
*     ORGANIZATION IS INDEXED
*     ACCESS MODE IS DYNAMIC
*     RECORD KEY COD-FILIALE
*     STATUS WS-STATUS.
     SELECT PRIMOF   ASSIGN TO 'AUTO.IDX'
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
* FD   SECONDOF
*      LABEL RECORD IS OMITTED
*      DATA RECORD IS REC-SECONDOF.
* 01 REC-SECONDOF.
*    05 COD-FILIALE2   PIC 9(4).
*    05 DESC-FILIALE2  PIC X(16).

************************************************************
*                WORKING-STORAGE SECTION                     
************************************************************

 WORKING-STORAGE SECTION.   
 01   WS-PRIMOF.
      05 WS-COD-FILIALE   PIC 9(4).
      05 WS-DESC-FILIALE  PIC X(16).
 01   WS-SECONDOF.
      05 WS-COD-FILIALE2   PIC 9(4).
      05 WS-DESC-FILIALE2  PIC X(16).     
 01   USER-INPUT PIC 9999 VALUE 0.
 01   CONTINUA   PIC X.
 01   CHOICE     PIC 9 VALUE 0.
      
 
 01 WS-STATUS PIC XX.   
 01 FINE-FILE PIC X VALUE SPACES.
************************************************************
*                PROCEDURE DIVISION                    
************************************************************
 PROCEDURE DIVISION.    
      
 MAIN-LINE.
      PERFORM APERTURA THRU FINE-APERTURA.
      PERFORM ELABORA THRU FINE-ELABORA UNTIL CONTINUA = 'N' or 
      'n'.
      PERFORM CHIUSURA THRU FINE-CHIUSURA.
      
      STOP RUN.
 
 APERTURA.  
 
     OPEN I-O PRIMOF.
*******************************************************           
*     MOVE 2004   TO COD-FILIALE.                     *
*     READ PRIMOF INTO WS-PRIMOF INVALID KEY          *
*                                                     *
*          DISPLAY 'ERRORE'                           *
*          DISPLAY WS-STATUS                          *
*     NOT INVALID KEY                                 *
*            DISPLAY 'RECORD LETTO'                   *
*            DISPLAY WS-PRIMOF.                       *
*******************************************************          
 FINE-APERTURA.   
      EXIT.
* 
 ELABORA.
     DISPLAY 'CHE OPERAZIONE VUOI ESEGUIRE?'
     ' SCRIVI 1 PER AGGIUNGERE RECORD, 2 PER MODIFICARE, 3 PER '
     'ELIMINARE, 4 PER LEGGERE'.
     ACCEPT CHOICE.
     EVALUATE CHOICE
      WHEN 1
*     IF CHOICE = 1
            DISPLAY 'DAMMI UN ID E UNA DESCRIZIONE'
            DISPLAY 'ID'
            ACCEPT WS-COD-FILIALE
            DISPLAY 'DESCRIZIONE'
            ACCEPT WS-DESC-FILIALE
            WRITE REC-PRIMOF FROM WS-PRIMOF INVALID KEY
                  DISPLAY WS-STATUS 
                  DISPLAY 'ERRORE'
          NOT INVALID KEY     
                  DISPLAY 'RECORD CREATO'     
                  DISPLAY WS-PRIMOF
*     END-IF.
      WHEN 2
*     IF CHOICE = 2
            DISPLAY 'DAMMI ID DI CHI VUOI MODIFICARE'
            ACCEPT COD-FILIALE
            READ PRIMOF INTO WS-PRIMOF INVALID KEY
                  DISPLAY 'ERRORE'
                  DISPLAY WS-STATUS
            NOT INVALID KEY      
            DISPLAY WS-PRIMOF
            DISPLAY 'DIMMI LA DESCRIZIONE NUOVA'
            ACCEPT WS-DESC-FILIALE
            REWRITE REC-PRIMOF FROM WS-PRIMOF INVALID KEY
                  DISPLAY 'ERRORE'
                  DISPLAY WS-STATUS
            NOT INVALID KEY
                  DISPLAY 'RECORD MODIFICATO'
                  DISPLAY WS-PRIMOF 
      WHEN 3            
*     END-IF.
*     IF CHOICE = 3
            DISPLAY 'DAMMI ID CHE VUOI ELIMINARE'
            ACCEPT COD-FILIALE
            READ PRIMOF INTO WS-PRIMOF INVALID KEY
                  DISPLAY 'ERRORE'
                  DISPLAY WS-STATUS
            NOT INVALID KEY
                  DISPLAY WS-PRIMOF ' ELIMINATO'
            DELETE PRIMOF      
      WHEN 4      
*     END-IF.
*     IF CHOICE = 4
            DISPLAY 'DAMMI ID CHE VUOI LEGGERE'
            ACCEPT COD-FILIALE
            READ PRIMOF INTO WS-PRIMOF INVALID KEY
                  DISPLAY 'ERRORE'
                  DISPLAY WS-STATUS
            NOT INVALID KEY
                  DISPLAY WS-PRIMOF ' RECORD TROVATO'
*     END-IF.
      WHEN OTHER
           DISPLAY 'BO, ERANO CHIARE LE ISTRUZIONI'.
           
     DISPLAY 'VUOI CONTINARE? SE NO PREMI N'.
     ACCEPT CONTINUA.
 FINE-ELABORA.
      EXIT.
* 
 CHIUSURA. 
 
     CLOSE PRIMOF.
            
           
 FINE-CHIUSURA.    
      EXIT.