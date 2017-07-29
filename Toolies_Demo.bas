CLS

FN.DEF ExtractLatitude(location$)
 ! we need to extract latitude from location "(Latitude, Longitude)"
 lat$=""
 IF location$<>"" THEN 
  ! split location by ";" delimeter and store fields in spl$[] array
  UNDIM spl$[]
  SPLIT spl$[], location$,";"
  lat$=spl$[1]
  ! Remove "(" from the beginning of lat$
  lat$=RIGHT$(lat$,LEN(lat$)-1)
  FN.RTN VAL(lat$ )  
 ENDIF

FN.END

FN.DEF ExtractLongitude(location$)
 ! we need to extract longitude from location "(Latitude, Longitude)"
 long$=""
 IF location$<>"" THEN 
  ! split location by ";" delimeter and store fields in spl$[] array
  UNDIM spl$[]
  SPLIT spl$[], location$,";"
  long$=spl$[2]
  ! Remove ")" from the end of long$
  long$=LEFT$(long$,LEN(long$)-1)
  FN.RTN VAL(long$)
 ENDIF

FN.END

FN.DEF degreesToRadians(degrees)
 FN.RTN degrees * PI() / 180
FN.END

FN.DEF GpsDistance(lat1, lon1, lat2, lon2) 
 earthRadiusKm = 6371

 dLat = degreesToRadians(lat2-lat1)
 dLon = degreesToRadians(lon2-lon1)

 lat1 = degreesToRadians(lat1)
 lat2 = degreesToRadians(lat2)

 a = SIN(dLat/2) * SIN(dLat/2) + SIN(dLon/2) * SIN(dLon/2) * COS(lat1) * COS(lat2)
 c = 2 * ATAN2(SQR(a), SQR(1-a)) 
 FN.RTN earthRadiusKm * c

FN.END

v_error_back=0

! Create and load rec$[]  
GOSUB GET_DATA

START:

CLS
! Setup your Menu
ARRAY.LOAD menus$[] ~
"List All Schools In ACT"~
"List ACT Schools by Suburbs"~
"List ACT Schools by Radius from Current Location"~
"Display ACT School Map Location by Radius from Current Location"~
"Exit"


! Set the Popup Message
msg$ ="Please Select a List: "

! Shows the list and waits for the user
! to make the selection.
DIALOG.SELECT menu, menus$[], msg$

SW.BEGIN menu

 SW.CASE 1
  GOSUB LIST_ALL
  SW.BREAK

 SW.CASE 2
  GOSUB LIST_SUBURBS
  SW.BREAK

 SW.CASE 3
  GOSUB LIST_RADIUS
  SW.BREAK

 SW.CASE 4
  GOSUB SCHOOL_MAP_RADIUS
  SW.BREAK

 SW.CASE 5
  EXIT  

 SW.DEFAULT
  GOTO start

SW.END


GOTO start


END


GET_DATA:

! open file

fl$="ACT_School_Locations.csv"

TEXT.OPEN R, FN1, fl$

! first line which is the header line
TEXT.READLN FN1, a_line$

! find out how may fields in the header
! store value in fld_num

UNDIM spl$[]
SPLIT spl$[], a_line$,","
ARRAY.LENGTH fld_num, spl$[]


! let us find out how many records we have in the file
! store value in rec_num

rec_num=0
TEXT.READLN FN1, a_line$
WHILE a_line$ <> "EOF"
 rec_num = rec_num +1
 TEXT.READLN FN1, a_line$
REPEAT

! close file
TEXT.CLOSE FN1


!!

create array rec$[] to capture the following data
Field 1: School Name
Field 2: Street Address
Field 3: Suburb
Field 4: Sector
Field 5: Type
Field 6: Location

!!

DIM rec$[rec_num,fld_num]

! open file
TEXT.OPEN R, FN1, fl$

! skip first line which is title line
TEXT.READLN FN1, a_line$

FOR I = 1 TO rec_num
 TEXT.READLN FN1, a_line$

 ! clear array spl$[] 
 UNDIM spl$[]
 ! split each record line by comma delimeter and store fields in spl$[] array
 ! eg school_code will be stored in spl$[1],  school_name will be stored in spl$[3]


 SPLIT spl$[], a_line$,","
 ! If the last column is blank, we will be short on a column
 ! The next command will return the number of columns

 ARRAY.LENGTH flds ,spl$[]

 ! load each record in rec$[I,j]
 ! rec$[1,1] will store the School Name for the first record
 ! rec$[1,2] will store the Street Address for the first record
 ! rec$[1,3] will store the Suburb for the first record
 ! rec$[1,4] will store the Sector for the first record
 ! rec$[1,5] will store the Type for the first record
 ! rec$[1,6] will store the Location for the first record

 ! rec$[2,1] will store the School Name for the first record
 ! rec$[2,2] will store the Street Address for the first record
 ! rec$[2,3] will store the Suburb for the first record
 ! rec$[2,4] will store the Sector for the first record
 ! rec$[2,5] will store the Type for the first record
 ! rec$[2,6] will store the Location for the first record


 FOR j =1 TO fld_num
  IF j > flds THEN
   ! If the last column is blank we are filling the corresponding array entry with blank  
   rec$[I,j] =""
  ELSE
   rec$[I,j] =spl$[j]
  ENDIF

 NEXT j
NEXT I

! close file
TEXT.CLOSE FN1
RETURN


LIST_ALL:

FOR i = 1 TO rec_num
 PRINT "School Name: "; rec$[i,1];", "; "Street Address: "; rec$[i,2];", ";"Suburb: "; rec$[i,3];", ";"Sector: "; rec$[i,4];", ";"Type: "; rec$[i,5];", ";"Location: "; rec$[i,6]
NEXT i


IF v_error_back =1 THEN BACK.RESUME

GOTO WaitLoop

RETURN

LIST_SUBURBS:

INPUT "Enter Suburb",PSUBURB$

FOR i = 1 TO rec_num
 IF UPPER$(rec$[i,3])=UPPER$(PSUBURB$) THEN
  PRINT "School Name: "; rec$[i,1];", "; "Street Address: "; rec$[i,2];", ";"Suburb: "; rec$[i,3];", ";"Sector: "; rec$[i,4];", ";"Type: "; rec$[i,5];", ";"Location: "; rec$[i,6]
 ENDIF
NEXT i

IF v_error_back =1 THEN BACK.RESUME

GOTO WaitLoop


RETURN

LIST_RADIUS:

GPS.OPEN

GPS.LATITUDE latitude
! PRINT  "Latitude:   " + FORMAT$("##%.#####", latitude)

GPS.LONGITUDE longitude
! PRINT"Longitude:  " + FORMAT$("##%.#####", longitude)


IF ABS(Latitude) = 0  | ABS(longitude) =0 THEN

 latitude= -35.278101           
 longitude=149.147123    

 DIALOG.MESSAGE "No GPS Data Available", "Using Default Location -35.278101 , 149.147123 ",go,"ok"

ENDIF


Enter_Radius1:
INPUT "Enter Search Radius in Kms",KMS$
IF !IS_NUMBER(KMS$) THEN GOTO Enter_Radius1
km=VAL(kms$)


FOR i = 1 TO rec_num
 ! we need to extract latitude and longitude from rec$[i,6]
 IF rec$[i,6]<>"" THEN
  position$=rec$[i,6]

  lat = ExtractLatitude(position$)
  long =  ExtractLongitude(position$)

  IF ABS(GpsDistance(latitude, longitude, lat,long) ) <= km THEN
   PRINT "School Name: "; rec$[i,1];", "; "Street Address: "; rec$[i,2];", ";"Suburb: "; rec$[i,3];", ";"Sector: "; rec$[i,4];", ";"Type: "; rec$[i,5];", ";"Location: "; rec$[i,6]
  ENDIF
 ENDIF


NEXT i



GPS.CLOSE

IF v_error_back =1 THEN BACK.RESUME

GOTO WaitLoop

RETURN

SCHOOL_MAP_RADIUS:
GPS.OPEN

GPS.LATITUDE latitude
! PRINT  "Latitude:   " + FORMAT$("##%.#####", latitude)

GPS.LONGITUDE Longitude
! PRINT"Longitude:  " + FORMAT$("##%.#####", longitude)


IF ABS(Latitude) = 0  | ABS(longitude) =0 THEN

 latitude= -35.278101           
 longitude=149.147123    

 DIALOG.MESSAGE "No GPS Data Available", "Using Default Location -35.278101 , 149.147123 ",go,"ok"

ENDIF


Enter_Radius2:
INPUT "Enter Search Radius in Kms",KMS$
IF !IS_NUMBER(KMS$) THEN GOTO Enter_Radius2
km=VAL(kms$)

! Create a new Array to hold all schools in the selected radius

DIM schools$[rec_num,5]
I=0

FOR j = 1 TO rec_num
 ! we need to extract latitude and longitude from rec$[i,6]
 IF rec$[j,6]<>"" THEN
  position$=rec$[j,6]

  lat = ExtractLatitude(position$)
  long =  ExtractLongitude(position$)

  IF ABS(GpsDistance(latitude, longitude, lat,long) ) <= km THEN

   I=I+1
   schools$[I,1]=rec$[j,1]
   schools$[I,2]=rec$[j,2]
   schools$[I,3]=rec$[j,3]
   schools$[I,4]=STR$(lat)
   schools$[I,5]=STR$(long)  
  ENDIF
 ENDIF


NEXT j


GPS.CLOSE

DIM map$[i+1]

FOR j = 1 TO I
 map$[j] = schools$[j,1]  +", "+schools$[j,2]+", "+schools$[j,3]
NEXT j

map$[j]="Exit"

MAP_MENU:

DIALOG.SELECT map, map$[], msg$
IF map = j THEN
 RETURN
ELSE
 url$="http://maps.google.com/?q=" + schools$[map,4]+","+schools$[map,5]
 BROWSE url$
 GOTO MAP_MENU
ENDIF


RETURN


WaitLoop:

w=0
DO
 PAUSE 10
UNTIL w=1


ONBACKKEY:
v_error_back =1
RETURN
