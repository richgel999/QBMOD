'QBMP16.BAS (experiment 27-parallel substager/mixer & looping overmix)
'A Sample PDS/QB4.5 4-Channel Amiga MOD Player
'Written by Rich Geldreich (C) Copyright December 6, 1992
'You may use this program for anything you wish, but if you're going to
'make any money off it I would like to know about it first.
'
'NOTES: This program also requires INTRPT.OBJ (from QB.LIB or QBX.LIB),
'and QBMPASM.ASM to function. I have only tested this program in
'PDS 7.1. I *highly* recommend that you only run this program compiled!
'
'To compile(this is a pretty complicated procedure for beginners):
'-First, you must extract INTRPT.OBJ from QB.LIB or QBX.LIB(depending
'-on QB/BC7). Type:
'LIB QBX.LIB *INTRPT.OBJ,;              [BC7]
'LIB QB.LIB *INTRPT.OBJ,;               [QB4.5]
'-Then make a QLB by:
'LINK /q QBMPASM+INTRPT,,,QBXQLB,;      [BC7]
'LINK /q QBMPASM+INTRPT,,,BQLB45        [QB4.5]
'-And a LIB by:
'LIB QBMPASM QBMPASM+INTRPT,;           [BC7 or QB4.5]
'-Then load QBX(or QB) with the following and compile:
'QB(x) /lQBMPASM QBMP16
'Please note that the above instructions for QB4.5 haven't been tested.
'
'If you have a fast computer and want better sound change the variable
'"IntRate=11000" below to a larger number(max is about 25,600 hz!!)
'This program is currently configured to load MODs up to about 370k,
'change the line that says "Null& = SETMEM(-400000)" to load larger
'MODs. Anyone having problems or questions and can afford a L/D call:
'(609)-742-8752 between 3:00pm and 1:00am eastern time.
'
'If you get it compiled successfully, then run it with:
'QBMP16 modfile.MOD
'You should hear the MOD play on your PC-Speaker. The keys 1-4 turn on/
'off each channel, and the left and right arrows fast forward/rewind the
'MOD. Press escape to drop back to DOS. Here goes!! -RG
DEFINT A-Z
CONST True = -1, False = 0

DECLARE FUNCTION CheckDoneFlag% ()
DECLARE SUB SetBuffers (BYVAL BDS%, BYVAL Buffer1%, BYVAL Buffer2%,_
BYVAL BufferSize%)
DECLARE FUNCTION GetCS% ()
DECLARE FUNCTION GetOF% ()
DECLARE FUNCTION UnsignedComp% (BYVAL A%, BYVAL B%)

DECLARE SUB SetInt8Rate (A&)
DECLARE SUB SpeakerOff ()
DECLARE SUB SpeakerOn ()
DECLARE FUNCTION Alloc% (A%)
DECLARE SUB ExitWithError (A$)
DECLARE FUNCTION Extract% (A$, offset%)

TYPE RegTypeX
     ax    AS INTEGER
     bx    AS INTEGER
     cx    AS INTEGER
     dx    AS INTEGER
     bp    AS INTEGER
     si    AS INTEGER
     di    AS INTEGER
     flags AS INTEGER
     ds    AS INTEGER
     es    AS INTEGER
END TYPE
DIM SHARED CPU as RegTypeX

'Sample arrays.
DIM S.Name(30) AS STRING * 22
DIM S.Volume(30)
DIM S.Segment(30)
DIM S.RepStart(30),     S.RepLength(30)
DIM S.Length(30)

'Channel arrays
DIM C.InactiveFlag(3)
DIM C.Volume(3)
DIM C.Segment(3)
DIM C.Period(3),        C.HighStep(3),  C.LowStep(3)
DIM C.Offset(3),        C.Remainder(3)
DIM C.RepStart(3),      C.RepLength(3), C.LoopEnd(3)
DIM C.Length(3)

'For command processing
DIM C.Command(3)
DIM C.PortSpeed(3)
DIM C.PortDest(3)
DIM C.VolumeSpeed(3)

'Misc. Arrays
DIM PeriodHigh(1023),   PeriodLow(1023) 'Precalculated step rates.
DIM VolumeTable(63, 255) 'Precalculated volume tables.
DIM SampleBuffer(1199)   'buffer for mixing+playing at same time.
DIM PatternSegment(127)  'Holds segments of all the patterns to play.
DIM ChannelOn(3)

DIM Scale8to6(255)       'Translation table for dividing each signed
                         'sample by 4.
'=====================================================================
'Modify this variable for better sound(controls how many samples are
'mixed per second).
IntRate = 12000

ON ERROR GOTO ErrorHandler

PRINT "QBMP v1.6 - An Amiga MOD player written in PDS/QuickBASIC."
PRINT "(C) Copyright 1992 By Rich Geldreich"

'Precalculate an 8-bit to 6-bit signed translation table, to translate
'the signed 8 bit samples in the MOD to signed 6 bit samples for mixing.
FOR A=-128 to 127
    C=A\4:IF A<0 then Scale8to6(A+256)=C ELSE Scale8to6(A)=C
NEXT

'Free up 400,000 bytes of far memory(increase this if you get an out of
'memory error while loading big MODs).
Null& = SETMEM(-400000)

FileSpec$ = Command$
IF INSTR(FileSpec$,".")=0 THEN FileSpec$=FileSpec$+".MOD"
'Attempt to open the MOD file.
OPEN FileSpec$ FOR INPUT AS #1:CLOSE #1
OPEN FileSpec$ FOR BINARY AS #1

'Check to see if MOD contains 15 or 31 samples...
A$ = "    ": GET #1, 1081, A$
'If the string at offset 1081 is "M.K.", or the first 3 letters are
'"FLT", then the MOD contains 31 samples:
S.Max = 15 - 16 * ((A$ = "M.K.") OR (LEFT$(A$, 3) = "FLT"))

'Print the MOD's title
A$ = SPACE$(20): GET #1, 1, A$:PRINT "Title: ";A$
'=====================================================================
A$ = SPACE$(8)
FOR A = 0 TO S.Max - 1
    'Skip the sample's name
    GET #1, , S.Name(A)
    'Get the info on the sample
    GET #1, , A$
    S.Volume(A)         = ASC(MID$(A$, 4, 1))
    IF S.Volume(A) > 64 THEN S.Volume(A) = 64
    S.Length(A)         = Extract(A$, 1)
    S.RepStart(A)       = Extract(A$, 5)
    S.RepLength(A)      = Extract(A$, 7)
    IF S.RepLength(A) = 2 THEN S.RepLength(A) = 0
    IF S.Length(A)=2 THEN S.Length(A)=0

'Check the sample's loop end to make sure it's in range.
    A&=S.RepStart(A)+S.RepLength(A)-1:IF A&<0 THEN A&=A&+65536
    B&=S.Length(A):IF B&<0 THEN B&=B&+65536
    IF A&>B& THEN 'If not in range, then divide by two(DigiStudio does
        A&=S.RepStart(A):IF A&<0 THEN A&=A&+65536     'this). ?
        A&=A&\2:S.RepStart(A) = A&
    END IF

NEXT
'=====================================================================
A$ = " ": GET #1, , A$: T.Length = ASC(A$): GET #1, , A$
'Load the pattern table, and find the highest pattern to load.
HighestPattern = -1
FOR A = 0 TO 127
    GET #1, , A$: B = ASC(A$)
    IF B > HighestPattern THEN HighestPattern = B
    PatternSegment(A) = B
NEXT
'=====================================================================
IF S.Max = 31 THEN SEEK #1, LOC(1) + 5
A$ = SPACE$(1024)
'Load the patterns.
FOR A = 0 TO HighestPattern
    LOCATE , 1: PRINT USING "Parsing Pattern ##"; A;
    GET #1, , A$
    B = Alloc(80): DEF SEG = B: C = 0: e = 1
    FOR d = 1 TO 256
        b1 = ASC(MID$(A$, e, 1)): b2 = ASC(MID$(A$, e + 1, 1))
        b3 = ASC(MID$(A$, e + 2, 1)): b4 = ASC(MID$(A$, e + 3, 1))
        e = e + 4
        sample = (b1 AND 240) OR (b3 \ 16)
        period = (b1 AND 15) * 256 OR b2
        effect = b3 AND 15:operand = b4

        IF sample > S.Max THEN sample = 0
        IF period > 1023 OR period < 20 THEN period = 0

        SELECT CASE effect
        CASE &HC
            IF operand > 64 THEN operand = 64
        END SELECT
        'Store the channel in an expanded format for ease of
        'processing during playing.
        POKE C, sample
        POKE C + 1, period \ 256
        POKE C + 2, period AND 255
        POKE C + 3, effect
        POKE C + 4, operand
        C = C + 5
    NEXT
    'Fill in the pattern's segment in the position table.
    FOR d = 0 TO 127:IF PatternSegment(d)=A THEN PatternSegment(d)=B
    NEXT
NEXT
'=====================================================================
'Load the samples.
FOR A = 0 TO S.Max - 1
    LOCATE , 1: PRINT USING "Loading Sample ##  "; A + 1;

    d& = S.Length(A): IF d& < 0 THEN d& = d& + 65536
    'Allocate 1024 bytes more than needed for mixer runoff.
    d& = d& + 1024
    IF d& > 65500 THEN ExitWithError "Sample Too Large"

    B = Alloc(d& \ 16 + 1) 'Allocate memory for the sample.
    S.Segment(A) = B

    'Load the sample
    CPU.ax = &H3F00
    CPU.bx = FILEATTR(1, 2)
    CPU.ds = B: CPU.dx = 0
    CPU.cx = S.Length(A)

    CALL interruptx(&H21, CPU, CPU)
    IF (CPU.Flags AND 1) THEN ExitWithError "Error Loading Sample"

    DEF SEG = B

    A& = S.Length(A): IF A& < 0 THEN A& = A& + 65536

    'Divide each byte of the sample by 4 for mixing. A lookup table
    'is used because QB doesn't support signed bytes.
    FOR B& = 0 TO A& - 1
        POKE B&, Scale8to6(PEEK(B&))
    NEXT

    'Clear the end of the sample for mixer runoff.
    FOR A& = A& TO A& + 1023:POKE A&, 0:NEXT
NEXT
'=====================================================================
LOCATE ,1:PRINT SPACE$(40);

'Figure out how many samples are mixed per 1/50th of a second.
IntsPerClick = IntRate \ 50

'Precalculate a step for each period. The constant &H369040 is from
'the Amiga, it is scaled up by 256 (&h100) so floating point math can
'be eliminated.
K& = &H36904000 \ IntRate
FOR A = 20 TO 1023
    A& = K& \ A
    PeriodHigh(A) = A& \ 256
    PeriodLow(A) = CINT(A&) AND 255
NEXT
'Precalculate the volume lookup tables. Enables the mixer to adjust
'the volume of a sample without slow multiples and divides.
FOR A = 0 TO 63
    FOR B = -128 TO -1
        C = (B * A) \ 64: IF C < 0 THEN C = C + 256
        VolumeTable(A, B + 256) = C
    NEXT
    FOR B = 0 TO 127:VolumeTable(A, B) = (B * A) \ 64:NEXT
NEXT

'Make all channels currently inactive, but enable them(the keys 1-4
'toggle the ChannelOn() table to turn off/on channels).
FOR A = 0 TO 3: C.InactiveFlag(A) = True: ChannelOn(A)=True:NEXT

BufferOffset    = 512   'Current mixing offset.
T.Tempo         = 6     'Default tempo is 6/50th of a second.
T.ClicksLeft    = 6     'Clicks left before a line.
T.Pos           = 0     'Lines left before a new pattern.
GOSUB StartNewPattern
GOSUB DoLine

'Save old interrupt 8 handler.
CPU.ax = &H3508:CALL interruptx(&H21, CPU, CPU)
Old8.Offset = CPU.bx:Old8.Segment = CPU.es

'Initialize the assembly buffer player.
SetBuffers VARSEG(SampleBuffer(0)), VARPTR(SampleBuffer(0)), _
           VARPTR(SampleBuffer(512)), IntsPerClick * 2

'Set int 8 to our asm routine
CPU.ax = &H2508:CPU.ds = GetCS:CPU.dx = GetOF
CALL interruptx(&H21, CPU, CPU)

'Reprogram the 8255's timer to the specified sample rate.
SetInt8Rate &H1234DE \ IntRate

'Turn speaker on, and play the MOD.
SpeakerOn
DO
    'Wait for sync signal from the assembly buffer player...
    DO: LOOP UNTIL CheckDoneFlag

    'Mix more samples, to keep the MOD going.
    GOSUB DoMix

    A$=INKEY$
    IF A$<>"" THEN  'Process any keystrokes.
        K=ASC(RIGHT$(A$,1))
        SELECT CASE K
        CASE 49 TO 52   '1-4
    	    ChannelOn(K-49) = NOT ChannelOn(K-49)
        CASE 27         'Escape
    	    Exit do
        CASE &H4b       'Left
            T.Pos=T.Pos-1
            IF T.Pos<0 THEN T.Pos=T.Length-1
            GOSUB StartNewPattern
        CASE &H4d       'Right
            T.Pos=T.Pos+1
            IF T.Pos=>T.Length THEN T.Pos=0
            GOSUB StartNewPattern
        END SELECT
   END IF

LOOP
'Turn off speaker.
SpeakerOff
'Set int 8 rate to normal (18.2 hz)
SetInt8Rate 0

'restore old int 8 handler
CPU.ax = &H2508:CPU.ds = Old8.Segment:CPU.dx = Old8.Offset
CALL interruptx(&H21, CPU, CPU)

LOCATE ,1:PRINT SPACE$(40);
END
'=====================================================================
'Processes 1 line(4 channels) of a pattern.
DoLine:
    DEF SEG = PatternSegment

    FOR A = 0 TO 3
        C = PEEK(T.Offset): T.Offset = T.Offset + 1

        IF C <> 0 THEN  'Process a new sample, if any.
            C = C - 1
            'Load then new sample into the channels arrays.
            C.Segment(A)        = S.Segment(C)
            C.Volume(A)         = S.Volume(C)
            C.RepStart(A)       = S.RepStart(C)
            C.RepLength(A)      = S.RepLength(C)
            C.Length(A)         = S.Length(C)
            C.LoopEnd(A)        = C.Length(A)
        END IF

        C = PEEK(T.Offset) * 256 + PEEK(T.Offset + 1)
        T.Offset = T.Offset + 2
        IF C <> 0 THEN  'Process a new period, if any.
            IF PEEK(T.Offset) <> 3 THEN
                C.Period(A)     = C
                C.HighStep(A)   = PeriodHigh(C) 'Lookup the step rate
                C.LowStep(A)    = PeriodLow(C)  'of the new period.
                C.Offset(A)     = 0             'Set current offset=0.
                C.LoopEnd(A)    = C.Length(A)
                C.Remainder(A)  = -256          'Set remainder to 0.
                C.InactiveFlag(A) = False       'Turn channel on.
            END IF
        END IF

        C.Command(A) = 0
        M = PEEK(T.Offset)
        IF M<>0 THEN    'Process a command, if any.
            o = PEEK(T.Offset + 1)
            SELECT CASE M
            CASE 12             'Volume
                C.Volume(A) = o
            CASE 15             'Tempo
                T.Tempo = o
            CASE 1              'Port Down
                C.Command(A)   = 2
                C.PortSpeed(A) = O
            CASE 2              'Port Up
                C.Command(A)   = 3
                C.PortSpeed(A) = O
            CASE 3              'Port to Note
                IF C.Period(A) > C THEN
                    C.Command(A) = 4
                ELSE
                    C.Command(A) = 5
                END IF
                '!!!fix
                'If no port destination, then use last.
                IF C=0 THEN C=C.PortDest(A)
                IF C<113 THEN C=113 ELSE IF C>1023 THEN C=1023

                C.PortSpeed(A) = o
                C.PortDest(A)  = C
            CASE 10             'Volume Slide
                C.Command(A) = 1
                IF O AND 15 THEN
                    C.VolumeSpeed(A) = -(O AND 15)
                ELSE
                    C.VolumeSpeed(A) = O\16
                END IF
            CASE 11             'Position Jump
                IF o<T.Length THEN
                    T.Pos = o
                    T.Line = 1
                END IF
            CASE 13             'Pattern Skip
                T.Line = 1
            END SELECT
        END IF
        T.Offset = T.Offset + 2
    NEXT
    T.ClicksLeft = T.Tempo
    T.Line = T.Line - 1:IF T.Line = 0 THEN GOTO NewPattern
RETURN
NewPattern:
    T.Pos = T.Pos + 1:IF T.Pos >= T.Length THEN T.Pos = 0
    GOSUB StartNewPattern
RETURN
StartNewPattern:
    LOCATE ,1
    PRINT USING "Playing:###"; (T.Pos*100&)\T.Length;
    PRINT "%";
    T.Line              = 64
    PatternSegment      = PatternSegment(T.Pos)
    T.Offset            = 0
RETURN
'=====================================================================
'Main mixer follows. While the assembly routine is playing one buffer,
'this routine mixes the other.
DoMix:
    BufferOffset = BufferOffset XOR 512

    Tm=0
    FOR K = 0 TO 3
        IF C.InactiveFlag(K)=False AND ChannelOn(K) THEN
            GOSUB Mix
            Tm = 1
        END IF
    NEXT
    If Tm=0 then 'If all channels inactive, just clear buffer to 0's.
        FOR B = BufferOffset TO BufferOffset + IntsPerClick-1
            SampleBuffer(B) = 0
        NEXT
    END IF

    T.ClicksLeft = T.ClicksLeft - 1
    'If not time for a new line then process slide commands, if any.
    IF T.ClicksLeft = 0 THEN GOSUB DoLine ELSE GOSUB DoCommands
RETURN
'=====================================================================
Mix:
    MixesLeftToDo   = IntsPerClick
    OffsetNow       = BufferOffset

    'Preload all needed variables for speed.
    DEF SEG = C.Segment(K)
    o = C.Offset(K)                     'current offset into sample
    r = C.Remainder(K)                  'current remainder(0-255) at offset
    v = C.Volume(K)                     'volume(0-64)

    h = C.HighStep(K)                   'integer step
    l = C.LowStep(K)                    'remainder step(0-255)

    IF C.RepLength(K) THEN
        DO
         'If sample loops then calculate the number of mixes left until
         'the loop occurs...
            PL&=C.LoopEnd(K)-o

         'Thanks to QB's lack of unsigned ints, we must do this...
            IF PL&<0 THEN PL&=PL&+65536

            MixesTillRepeat&=(PL&*256-(r+256)) \ ((h * 256&) + l)

         'If there is any remainder left, then add 1 to MixesTillRepeat.
            IF ( (PL& * 256 - (r+256)) MOD ((h * 256&) + l) ) THEN
                MixesTillRepeat&=MixesTillRepeat&+1
            END IF
         'If we must loop in this pass, then mix whatever is left before
         'the loop, and then update our sample pointer to the start of
         'the loop.
            IF MixesLeftToDo >= MixesTillRepeat& THEN

                MixesToDo      = MixesTillRepeat&
                MixesLeftToDo  = MixesLeftToDo - MixesToDo

                'Mix whatever is left before the loop...
                GOSUB LowLevelMix

               'Throw back our pointer to the loop start.
                IF C.LoopEnd(K)  = C.Length(K) THEN'If first time,setup
                    o            = C.RepStart(K)    'for next loop.
                    C.LoopEnd(K) = C.RepStart(K) + C.RepLength(K)
                ELSE
                    o            = o - C.RepLength(K)
                ENDIF
            ELSE
                EXIT DO 'Doesn't loop in this pass, just exit.
            END IF
        LOOP '<--Check again for VERY short loop lengths.
    END IF

    'Mix whatever is left.
    MixesToDo = MixesLeftToDo
    Gosub LowLevelMix

    'Store back the offset and its remainder.
    C.Offset(K) = o
    C.Remainder(K) = r

'If sample doesn't loop, and the offset passed the end of the sample,
'then turn off the channel.
    IF C.RepLength(K) = 0 AND UnsignedComp(o, C.Length(K)) > 0 THEN
        C.InactiveFlag(K) = True
    END IF
RETURN

LowLevelMix:
    'Copies the samples from the instruments to the mixing buffer.
    'If this is the first copy (Tm=0), then just store the sample,
    'otherwise add it into the buffer.
    If Tm=0 then
        'Since most channels will have a volume of 64 (max), then
        'only use the volume lookup table (which is slow) when needed.
        IF v <> 64 THEN
            FOR OffsetNow = OffsetNow TO OffsetNow + MixesToDo - 1
                SampleBuffer(OffsetNow) = VolumeTable(v, PEEK(o))
                o = o + h
                'If remainder overflows, then increment the offset by
                '1 and adjust the remainder back down.
                r = r + l: IF r => 0 THEN r = r - 256: o = o + 1
            NEXT
        ELSE
            FOR OffsetNow = OffsetNow TO OffsetNow + MixesToDo - 1
                SampleBuffer(OffsetNow) = PEEK(o)
                o = o + h
                r = r + l: IF r => 0 THEN r = r - 256: o = o + 1
            NEXT
        END IF
    ELSE
        'Since this isn't the first mix(Tm<>0), we must add in the new
        'samples into the buffer. Since each sample is already divided
        'by 4, we don't have to divide the sample buffer by 4 when we're
        'all done, to increase speed[(A+B+C+D)/4 = (A/4+B/4+C/4+D/4)].
        IF v <> 64 THEN
            FOR OffsetNow = OffsetNow TO OffsetNow + MixesToDo - 1
                SampleBuffer(OffsetNow) = SampleBuffer(OffsetNow)+_
VolumeTable(v, PEEK(o))
                o = o + h
                r = r + l: IF r => 0 THEN r = r - 256: o = o + 1
            NEXT
        ELSE
            FOR OffsetNow = OffsetNow TO OffsetNow + MixesToDo - 1
                SampleBuffer(OffsetNow)=SampleBuffer(OffsetNow)+_
PEEK(o)
                o = o + h
                r = r + l: IF r => 0 THEN r = r - 256: o = o + 1
            NEXT
        END IF
    END IF
RETURN
'=====================================================================
'Process the MOD sliding commands...
DoCommands:
    FOR A=0 TO 3
        IF C.Command(A) THEN
            SELECT CASE C.Command(A)
            CASE 1                      'Volume slide
                C.Volume(A)=C.Volume(A)+C.VolumeSpeed(A)
                IF C.Volume(A)<0 THEN
                    C.Volume(A)=0
                    C.Command(A)=0
                ELSEIF C.Volume(A)>64 THEN
                    C.Volume(A)=64
                    C.Command(A)=0
                END IF
            CASE 2                      'Port down
                C.Period(A) = C.Period(A) - C.PortSpeed(A)
                IF C.Period(A)<113 THEN
                    C.Period(A)  = 113
                    C.Command(A) = 0
                END IF
                C.HighStep(A)   = PeriodHigh(C.Period(A))
                C.LowStep(A)    = PeriodLow(C.Period(A))
            CASE 3                      'Port up
                C.Period(A) = C.Period(A) + C.PortSpeed(A)
                IF C.Period(A)>1023 THEN
                    C.Period(A) = 1023
                    C.Command(A) = 0
                END IF
                C.HighStep(A)   = PeriodHigh(C.Period(A))
                C.LowStep(A)    = PeriodLow(C.Period(A))
            CASE 4                      'Port to Note Down
                C.Period(A) = C.Period(A) - C.PortSpeed(A)
                IF C.Period(A) <= C.PortDest(A) THEN
                    C.Period(A) = C.PortDest(A)
                    C.Command(A) = 0
                END IF
                C.HighStep(A)   = PeriodHigh(C.Period(A))
                C.LowStep(A)    = PeriodLow(C.Period(A))
            CASE 5                      'Port to Note Up
                C.Period(A) = C.Period(A) + C.PortSpeed(A)
                IF C.Period(A) >= C.PortDest(A) THEN
                    C.Period(A) = C.PortDest(A)
                    C.Command(A) = 0
                END IF
                C.HighStep(A)   = PeriodHigh(C.Period(A))
                C.LowStep(A)    = PeriodLow(C.Period(A))
            END SELECT
        END IF
    NEXT
RETURN
'=====================================================================
'Allocates memory from DOS.
FUNCTION Alloc (A)
    CPU.ax = &H4800:CPU.bx = A
    CALL interruptx(&H21, CPU, CPU)
    IF (CPU.Flags AND 1) THEN ExitWithError "Out of Memory"
    Alloc = CPU.ax
END FUNCTION
'=====================================================================
SUB ExitWithError (A$)
    IF POS(0) <> 1 THEN PRINT
    PRINT A$: END
END SUB
'=====================================================================
'Extracts a Motorola word from a string, and multiples it by 2.
FUNCTION Extract% (A$, offset)
    v& = (512&*ASC(MID$(A$,offset,1))+ASC(MID$(A$,offset+1, 1))*2&)
    IF v& > 65535 THEN ExitWithError "Sample Too Large"
    IF v& > 32767 THEN v& = v& - 65536
    Extract% = v&
END FUNCTION
'=====================================================================
'Reprograms the 8255 timer so it hits an int 8 at a different rate.
SUB SetInt8Rate (A&)
    OUT &H42, 2 + 4 + 16 + 32
    OUT &H40, A& AND 255
    OUT &H40, A& \ 256
END SUB
'=====================================================================
SUB SpeakerOff
    OUT &H61, INP(&H61) AND 252
END SUB
'=====================================================================
SUB SpeakerOn
    OUT &H61,INP(&H61) OR 3:OUT &H43,128+32+16:OUT &H42,0
    OUT &H42,0:OUT &H43,128+16
END SUB
'=====================================================================
ErrorHandler:
    SELECT CASE ERR
    CASE 7
        ExitWithError "Out of Memory Error"
    CASE 52, 53, 68
        ExitWithError "Bad File Name/File not found"
    CASE ELSE
        ExitWithError "Fatal Error"+STR$(ERR)+" has occured!"
    END SELECT
'END OF QBMP16.BAS====================================================
