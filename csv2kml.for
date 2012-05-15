      PROGRAM csv2kml
c
c     Author: Raymond Finzel
c     Date: 5/11/2012
c
c
c     Usage: 
c
c [-options] filename [latitude:longitude:altitude] [lines before data]
c     
c     -E : altitude column ON (default)
c     -e : altitude column OFF
c     
c     -f : convert altitude from feet to meters
c
c     -h : short usage statement
c

******VARIABLES FOR COMMAND LINE OPERATIONS
      INTEGER numOpts,numArgs
      PARAMETER(numOpts = 4, numArgs = 3)
      CHARACTER*50 argument,arguments(numArgs)
      LOGICAL switches(numOpts)
c     switches(1) = E, (2) = e , (3) = f, (4) = h


******VARIABLES FOR BUILDING KML File
      CHARACTER*72 preamble(50),body(50),postscript(50),name*50,line*50
      CHARACTER*72 kml
      REAL latitude, longitude, altitude
      INTEGER col1,col2,col3,skip,tabindex

******CONSTANTS
      REAL f2m
      PARAMETER(f2m = 0.3048)

******VARIABLES FOR PROGRAMMATIC OPERATIONS
      INTEGER J,T, filestatus
      CHARACTER single

******VARIABLES THAT ARE ACTUALLY FUNCTIONS
      INTEGER PARSECOLS
      CHARACTER*50 PARSENAME,REPEAT,SQUASH

******CRITICAL ASSIGNMENTS
      switches(1) = .true.
      switches(2) = .false.
      switches(3) = .false.
      switches(4) = .false.
      DO i=1,numArgs
         arguments(i) = " "
      END DO

      filestatus = 0
      tabindex = 0
      skip = 0
      col1 = 1
      col2 = 2
      COL3 = -1


      altitude = -1
******MAIN

c*INIT
      OPEN(UNIT = 12, FILE = "kml.dat", STATUS = "OLD")
c**Read preamble
      J = 1
      DO WHILE (trim(kml).ne."<!--/preamble-->")
         READ(12,'(A72)') kml
         preamble(J) = kml
         J = J+1
      END DO
      preamble(J-1)="999"
c**Read body
      J = 1
      DO WHILE (trim(kml).ne."<!--/body-->")
         READ(12,'(A72)') kml
         body(J) = kml
         J= J+1
      END DO
      body(J-1)="999"
c**Read postscript      
      J = 1
      DO WHILE (trim(kml).ne."<!--/postscript-->")
         READ(12,'(A72)') kml
         postscript(J) = kml
         J = J+1
      END DO
      postscript(J-1)="999"

      CALL CMD(switches,arguments)
      IF (switches(4)) CALL HELP()
      col1 = PARSECOLS(1,arguments(2))
      name = PARSENAME(arguments(1))

c*CONF      
      IF (col1.ne.-1) THEN
         col2 = PARSECOLS(2,arguments(2))
         IF (switches(1).and.(.not.switches(2)))
     &   col3 = PARSECOLS(3,arguments(2))
         IF (arguments(3).ne." ") READ(arguments(3),*) skip
      ELSE
         IF (switches(1).and.(.not.switches(2))) col3 = 3
         IF (arguments(2).ne." ") READ(arguments(2),*) skip
      END IF

      OPEN(UNIT = 39, FILE = arguments(1), STATUS = 'UNKNOWN')
      OPEN(UNIT = 40, FILE = TRIM(name) // ".kml", STATUS = 'UNKNOWN')

c*IO
      CALL SKIPLINES(39,skip)
      CALL COLUMNS(39,filestatus,col1,col2,col3,
     &        latitude,longitude,altitude)

c**write preamble
      J = 1

      DO WHILE (trim(preamble(J)).ne."999")
         IF(trim(preamble(J)).eq."<!--name-->")
     &        WRITE(40,*) (TRIM(REPEAT(CHAR(9),tabindex)) // name)
         CALL AUTHOR(40,preamble(J),tabindex)
         J = J+1
      END DO
c**write body

      J =1 
      DO WHILE (trim(body(J)).ne."999")
         IF(trim(body(J)).eq."<!--data-->") THEN
            DO WHILE (filestatus.eq.0)
               IF (altitude.ne.-1) THEN
                  If(switches(3)) altitude = altitude * f2m
                  WRITE(line,100), longitude,latitude,altitude
 100              FORMAT(2(F12.6,","),F12.6)
               ELSE
                  WRITE(line,101), longitude,latitude
 101              FORMAT(F12.6,",",F12.6)
               END IF
               line = SQUASH(line)
               WRITE(40,*) (REPEAT(CHAR(9),tabindex) // line)
               CALL COLUMNS(39,filestatus,col1,col2,col3,
     &              latitude, longitude, altitude)
            END DO
        END IF
        CALL AUTHOR(40,body(J),tabindex)
        J = J+1
      END DO

      J=1
      DO WHILE(trim(postscript(J)).ne."999")

         CALL AUTHOR(40,postscript(J),tabindex)
         J = J+1
      END DO


      END PROGRAM

******SUBROUTINES****************************************************
*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *
*********************************************************************

******AUTHOR*********************************************************
*Keep track of indentation for kml file                             *
*                                                                   *
*********************************************************************
      SUBROUTINE AUTHOR(file, line, tabindex)
      INTEGER file,tabindex
      CHARACTER*72 line
      IF(line(1:1).eq."<") THEN
         IF(line(2:2).ne."!".AND.line(2:2).ne."?") THEN
            IF(line(2:2).eq."/") THEN
               tabindex = tabindex - 1
               WRITE(file,'(A72)') 
     &         (trim(REPEAT(CHAR(9),tabindex)) // line)
            ELSE
               WRITE(file,'(A72)')
     &         (trim(REPEAT(CHAR(9),tabindex)) // line)
               tabindex = tabindex + 1
            END IF
         ELSE
            WRITE(file,'(A72)')
     &      (trim(REPEAT(CHAR(9),tabindex)) // line)
         END IF
      ELSE
         WRITE(file,'(A72)')
     &   (trim(REPEAT(CHAR(9),tabindex)) // line)
      END IF
      END

******REPEAT*********************************************************
*Repeats character C n times.                                       *
*                                                                   *
*********************************************************************
      CHARACTER*50 FUNCTION REPEAT(C,n)
      CHARACTER C
      CHARACTER*50 ret
      INTEGER n
      ret = ""
      DO i=1,n
         ret = ret // C
      END DO
      REPEAT = ret
      RETURN
      END
******CMD************************************************************
*Subroutine for processing command line arguments and returning     *
*options and string arguments to the main program                   *
*********************************************************************
      SUBROUTINE CMD(switches,arguments)
      INTEGER J
      CHARACTER*50 arguments(*),argument,single*1
      LOGICAL switches(*), hasOpts
      hasOpts = .false.
      IF(iargc().eq.0) sTOP 'ERR: No arguments. -h for usage'
      DO i = 1, iargc()
         CALL GETARG(i,argument)
         IF ((argument(1:1).eq. '-').AND.(argument(2:2).ne.' ')) THEN
            hasOpts = .true.
            J = 2
            single = argument(J:J)
            DO WHILE (single.ne.' ')
               IF(single.eq.'E') THEN
                  switches(1) = .true.
               ELSE IF(single.eq.'e') THEN
                  switches(2) = .true.
               ELSE IF(single.eq.'f') THEN
                  switches(3) = .true.
               ELSE IF(single.eq.'h') THEN
                  switches(4) = .true.
               END IF
               J = J+1
               single = argument(J:J)
            END DO
            IF (i.ne.1) STOP 'ERR: unexpected order, -h for usage'
         ELSE
            IF (hasOpts) THEN
               arguments(i - 1) = argument
            ELSE
               arguments(i) = argument
            END IF
         END IF
      END DO
*END CMD
      END

******COLUMNS*********************************************************
*Subroutine for reading data from columns, separated by any number of*
*columns of garbage data                                             *
*file = UNIT , col = column that contains data                       *
**********************************************************************
      SUBROUTINE COLUMNS(file,status,col1, col2, col3,lat,lon,alt)
      REAL lat, lon, alt
      INTEGER col1,col2,col3,file,J,status
      CHARACTER*50 columns1(col1),columns2(col2),columns3(col3)
      CHARACTER*512 line, single*1
      READ(file,'(A512)',IOSTAT = status) line
      J=1
      single = line(J:J)
      DO WHILE ((single.ne.CHAR(10)).and.(single.ne.CHAR(13)))
         IF ((single.eq.CHAR(9)).or.(single.eq.',')) THEN
            IF ((line(J+1:J+1).eq.'\t')
     &      .or.(line(J+1:J+1).eq.',')) THEN
               line = line(:J-1) // ' -999 ' // line(J+2:)
            ELSE
               line(J:J) = ' '
            END IF
         END IF
         J=J+1
         single = line(J:J)
      END DO
      IF (status.eq.0) THEN
         READ(line,*) columns1(1:col1-1), lat
         READ(line,*) columns2(1:col2-1), lon
         IF (col3.ne.-1) READ(line,*) columns3(1:col3-1),alt
c         print '(10A10,F8.3)',columns2(1:col2-1),lon
      END IF
*END COLUMNS
      END

******PARSECOLS*******************************************************
*Reads in a colon delimited field and spits out specified sub field  *
*                                                                    *
**********************************************************************

      INTEGER FUNCTION PARSECOLS(column, field)
      CHARACTER*50 field,temp,single*1
      INTEGER column, count
      INTEGER dat(column), J
      temp = field(1:len(field))
      count = 0
      J=1
      single = temp(1:1)
      DO WHILE (single.ne.' ')
         IF (single.eq.':') THEN
            temp(J:J)=' '
            count = count+1
         END IF
         J = J+1
         single = temp(J:J)
      END DO

      IF(column.gt.(count+1)) STOP 'ERR: Need more column data.'
      READ(temp,*) dat

      IF (count.ge.1) THEN
         PARSECOLS = dat(column)
      ELSE
         PARSECOLS = -1
      END IF
      RETURN
      END

******PARSENAME******************************************************
*Read file path, removing extension                                 *
*                                                                   *  
*********************************************************************
      CHARACTER*50 FUNCTION PARSENAME(name)
      CHARACTER*50 name, single*1
      INTEGER J
      J=1
      single = name(J:J)
      DO WHILE (single.ne.".")
         J = J+1
         single = name(J:J)
      END DO
      PARSENAME = name(1:J-1)
      PRINT *, PARSENAME
      RETURN
      END

******SKIPLINES******************************************************
*Performs a blank read on an open file. Skips lines from the file.  *
*                                                                   *
*********************************************************************

      SUBROUTINE SKIPLINES(file, numlines)
      INTEGER file, numlines
      DO i=1,numlines
         READ(file,*)
      END DO
      END

******SQUASH*********************************************************
*Remove spaces from between records.                                *
*                                                                   *
*********************************************************************

      CHARACTER*50 FUNCTION SQUASH(line)
      CHARACTER*50 line, temp, single*1
      INTEGER J, count
      J = 1
      count = 0
      temp = line(1:len(line))
      single = temp(1:1)
      DO WHILE (J.lt.len(line))
         DO WHILE ((single.eq.' ').and.(count.lt.len(temp)))
            count = count + 1
            temp(J:len(temp)) = temp(J+1:len(temp))
            single = temp(J:J)
         END DO
         count = 0
         J = J+1
         single = temp(J:J)
      END DO
      SQUASH = temp
      RETURN
      END

******HELP**********************************************************
*Print usage statement.                                            *
*                                                                  *
********************************************************************

      SUBROUTINE HELP()
      PRINT *,"Usage:"
      PRINT *, "[-options] filename [lat:lon:alt] [skip lines]"
      PRINT *, "Options:"
      PRINT *, " -E enable altitude column (ON by default)"
      PRINT *, " -e disable altitude column"
      PRINT *, " -f convert altitude from feet to meters"
      PRINT *, " -h print usage statement"
      STOP
      END








