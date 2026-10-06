       >>SOURCE FORMAT IS FREE
*> json2dir: create the directory tree a JSON document describes, in the current directory.
*> Implements RFC J2D-1 (https://github.com/kitsunoff/awesome-json2dir/blob/main/spec/rfc-json2dir.md).
*> Pure GnuCOBOL: hand-written UTF-8 check and JSON parser; the file system is reached
*> through CALLs to the C library (read, open, write, fchmod, mkdir, symlink, unlink, ...).
IDENTIFICATION DIVISION.
PROGRAM-ID. json2dir.

DATA DIVISION.
WORKING-STORAGE SECTION.
78 BUF-MAX VALUE 16777216.
78 NODE-MAX VALUE 400000.
78 STACK-MAX VALUE 1000.
78 PATH-MAX VALUE 65536.

*> raw input, decoded strings, and directory paths
01 IN-BUF       PIC X(16777216).
01 IN-LEN       PIC S9(9) COMP-5 VALUE 0.
01 POOL         PIC X(16777216).
01 POOL-LEN     PIC S9(9) COMP-5 VALUE 0.
01 PATHS        PIC X(16777216).
01 PATHS-LEN    PIC S9(9) COMP-5 VALUE 0.

*> one node per JSON value; nodes are stored in document order, so parents come first
01 NODE-COUNT   PIC S9(9) COMP-5 VALUE 0.
01 NODES.
   05 ND OCCURS 400000.
      10 ND-TYPE    PIC X.
      10 ND-PARENT  PIC S9(9) COMP-5.
      10 ND-NOFF    PIC S9(9) COMP-5.
      10 ND-NLEN    PIC S9(9) COMP-5.
      10 ND-VOFF    PIC S9(9) COMP-5.
      10 ND-VLEN    PIC S9(9) COMP-5.
      10 ND-POFF    PIC S9(9) COMP-5.
      10 ND-PLEN    PIC S9(9) COMP-5.
      10 ND-FIRST   PIC S9(9) COMP-5.
      10 ND-LAST    PIC S9(9) COMP-5.
      10 ND-NEXT    PIC S9(9) COMP-5.

01 STK OCCURS 1000 PIC S9(9) COMP-5.
01 SP           PIC S9(9) COMP-5 VALUE 0.

*> parser state
01 P            PIC S9(9) COMP-5.
01 W-CH           PIC X.
01 W-STATE      PIC X.
01 S-OFF        PIC S9(9) COMP-5.
01 S-LEN        PIC S9(9) COMP-5.
01 K-OFF        PIC S9(9) COMP-5.
01 K-LEN        PIC S9(9) COMP-5.
01 NAME-OFF     PIC S9(9) COMP-5.
01 NAME-LEN     PIC S9(9) COMP-5.
01 N            PIC S9(9) COMP-5.
01 PN           PIC S9(9) COMP-5.
01 C            PIC S9(9) COMP-5.
01 I            PIC S9(9) COMP-5.
01 J            PIC S9(9) COMP-5.
01 B            PIC S9(9) COMP-5.
01 NCONT        PIC S9(9) COMP-5.
01 LO           PIC S9(9) COMP-5.
01 HI           PIC S9(9) COMP-5.
01 CP           PIC S9(9) COMP-5.
01 CP2          PIC S9(9) COMP-5.
01 HX           PIC S9(9) COMP-5.
01 T            PIC S9(9) COMP-5.
01 CNT          PIC S9(9) COMP-5.
01 OUT-BYTE     PIC S9(9) COMP-5.

*> C library call arguments
01 W-FD           PIC S9(9) COMP-5.
01 FD0          PIC S9(9) COMP-5 VALUE 0.
01 RC           PIC S9(9) COMP-5.
01 SZ           PIC S9(18) COMP-5.
01 W-OFF        PIC S9(9) COMP-5.
01 W-REM        PIC S9(18) COMP-5.
01 O-FLAGS      PIC S9(9) COMP-5 VALUE 193.
01 O-MODE       PIC S9(9) COMP-5 VALUE 438.
01 D-MODE       PIC S9(9) COMP-5 VALUE 511.
01 ZERO-INT     PIC S9(9) COMP-5 VALUE 0.
01 OLD-MASK     PIC S9(9) COMP-5.
01 X-MODE       PIC S9(9) COMP-5.
01 DIGIT        PIC S9(9) COMP-5.
01 MBIT         PIC S9(9) COMP-5.
01 DIRP         USAGE POINTER.
01 EXIST        PIC X.
01 W-PATH       PIC X(65536).
01 PL           PIC S9(9) COMP-5.
01 T-BUF        PIC X(65536).
01 L-BUF        PIC X(4096).

01 ARGC         PIC 9(4).
01 ERR-MSG      PIC X(200).

PROCEDURE DIVISION.
MAIN-PARA.
    ACCEPT ARGC FROM ARGUMENT-NUMBER
    IF ARGC > 0
        DISPLAY "usage: json2dir < document.json" UPON SYSERR
        MOVE 2 TO RETURN-CODE
        STOP RUN
    END-IF
    PERFORM READ-INPUT
    PERFORM CHECK-UTF8
    PERFORM PARSE-DOC
    PERFORM APPLY-TREE
    MOVE 0 TO RETURN-CODE
    STOP RUN.

FAIL.
    DISPLAY "json2dir: " FUNCTION TRIM(ERR-MSG TRAILING) UPON SYSERR
    MOVE 1 TO RETURN-CODE
    STOP RUN.

FAIL-PATH.
    DISPLAY "json2dir: " W-PATH(1:PL) ": " FUNCTION TRIM(ERR-MSG TRAILING) UPON SYSERR
    MOVE 1 TO RETURN-CODE
    STOP RUN.

*> ---------------------------------------------------------------- input
READ-INPUT.
    PERFORM UNTIL 1 = 0
        COMPUTE SZ = BUF-MAX - IN-LEN
        IF SZ <= 0
            MOVE "input is too large" TO ERR-MSG
            PERFORM FAIL
        END-IF
        IF SZ > 65536
            MOVE 65536 TO SZ
        END-IF
        CALL "read" USING BY VALUE FD0 BY REFERENCE IN-BUF(IN-LEN + 1:SZ) BY VALUE SZ
            RETURNING RC
        IF RC < 0
            MOVE "cannot read standard input" TO ERR-MSG
            PERFORM FAIL
        END-IF
        IF RC = 0
            EXIT PERFORM
        END-IF
        ADD RC TO IN-LEN
    END-PERFORM.

*> §3: strict UTF-8 (no overlongs, no surrogates, nothing above U+10FFFF)
CHECK-UTF8.
    MOVE 1 TO I
    IF IN-LEN >= 3
        IF IN-BUF(1:3) = X"EFBBBF"
            MOVE 4 TO I
        END-IF
    END-IF
    MOVE I TO P
    PERFORM UNTIL I > IN-LEN
        COMPUTE B = FUNCTION ORD(IN-BUF(I:1)) - 1
        MOVE 128 TO LO
        MOVE 191 TO HI
        EVALUATE TRUE
            WHEN B < 128
                MOVE 0 TO NCONT
            WHEN B >= 194 AND B <= 223
                MOVE 1 TO NCONT
            WHEN B = 224
                MOVE 2 TO NCONT
                MOVE 160 TO LO
            WHEN B = 237
                MOVE 2 TO NCONT
                MOVE 159 TO HI
            WHEN B >= 225 AND B <= 239
                MOVE 2 TO NCONT
            WHEN B = 240
                MOVE 3 TO NCONT
                MOVE 144 TO LO
            WHEN B >= 241 AND B <= 243
                MOVE 3 TO NCONT
            WHEN B = 244
                MOVE 3 TO NCONT
                MOVE 143 TO HI
            WHEN OTHER
                PERFORM BAD-UTF8
        END-EVALUATE
        IF I + NCONT > IN-LEN
            PERFORM BAD-UTF8
        END-IF
        PERFORM VARYING J FROM 1 BY 1 UNTIL J > NCONT
            COMPUTE B = FUNCTION ORD(IN-BUF(I + J:1)) - 1
            IF J = 1
                IF B < LO OR B > HI
                    PERFORM BAD-UTF8
                END-IF
            ELSE
                IF B < 128 OR B > 191
                    PERFORM BAD-UTF8
                END-IF
            END-IF
        END-PERFORM
        COMPUTE I = I + 1 + NCONT
    END-PERFORM.

BAD-UTF8.
    MOVE "input is not valid UTF-8" TO ERR-MSG
    PERFORM FAIL.

*> ---------------------------------------------------------------- parser
GET-W-CH.
    IF P > IN-LEN
        MOVE X"00" TO W-CH
    ELSE
        MOVE IN-BUF(P:1) TO W-CH
    END-IF.

SKIP-WS.
    PERFORM GET-W-CH
    PERFORM UNTIL P > IN-LEN
            OR (W-CH NOT = SPACE AND W-CH NOT = X"09" AND W-CH NOT = X"0A" AND W-CH NOT = X"0D")
        ADD 1 TO P
        PERFORM GET-W-CH
    END-PERFORM.

BAD-JSON.
    MOVE "input is not valid JSON" TO ERR-MSG
    PERFORM FAIL.

EXPECT-QUOTE.
    PERFORM SKIP-WS
    IF W-CH NOT = '"'
        PERFORM BAD-JSON
    END-IF.

PARSE-DOC.
    PERFORM SKIP-WS
    IF W-CH NOT = "{"
        IF P > IN-LEN OR W-CH = '"' OR W-CH = "[" OR W-CH = "-" OR W-CH = "t" OR W-CH = "f"
                OR W-CH = "n" OR (W-CH >= "0" AND W-CH <= "9")
            MOVE "the root of the document must be an object" TO ERR-MSG
        ELSE
            MOVE "input is not valid JSON" TO ERR-MSG
        END-IF
        PERFORM FAIL
    END-IF
    ADD 1 TO P
    MOVE 1 TO NODE-COUNT
    MOVE "O" TO ND-TYPE(1)
    MOVE 0 TO ND-PARENT(1) ND-FIRST(1) ND-LAST(1) ND-NEXT(1) ND-PLEN(1)
    MOVE 1 TO SP
    MOVE 1 TO STK(1)
    MOVE "F" TO W-STATE
    PERFORM UNTIL SP = 0
        PERFORM SKIP-WS
        EVALUATE TRUE
            WHEN W-STATE = "A"
                *> after a member value: "," or "}"
                EVALUATE W-CH
                    WHEN ","
                        ADD 1 TO P
                        MOVE "N" TO W-STATE
                    WHEN "}"
                        ADD 1 TO P
                        SUBTRACT 1 FROM SP
                    WHEN OTHER
                        PERFORM BAD-JSON
                END-EVALUATE
            WHEN W-STATE = "F" AND W-CH = "}"
                ADD 1 TO P
                SUBTRACT 1 FROM SP
                MOVE "A" TO W-STATE
            WHEN OTHER
                PERFORM PARSE-MEMBER
        END-EVALUATE
    END-PERFORM
    PERFORM SKIP-WS
    IF P <= IN-LEN
        PERFORM BAD-JSON
    END-IF.

PARSE-MEMBER.
    IF W-CH NOT = '"'
        PERFORM BAD-JSON
    END-IF
    PERFORM PARSE-STRING
    MOVE S-OFF TO NAME-OFF
    MOVE S-LEN TO NAME-LEN
    PERFORM SKIP-WS
    IF W-CH NOT = ":"
        PERFORM BAD-JSON
    END-IF
    ADD 1 TO P
    PERFORM SKIP-WS
    PERFORM CHECK-NAME
    PERFORM ADD-NODE
    EVALUATE W-CH
        WHEN '"'
            PERFORM PARSE-STRING
            MOVE "S" TO ND-TYPE(N)
            MOVE S-OFF TO ND-VOFF(N)
            MOVE S-LEN TO ND-VLEN(N)
            MOVE "A" TO W-STATE
        WHEN "{"
            ADD 1 TO P
            MOVE "O" TO ND-TYPE(N)
            IF SP >= STACK-MAX
                MOVE "the document is nested too deeply" TO ERR-MSG
                PERFORM FAIL
            END-IF
            ADD 1 TO SP
            MOVE N TO STK(SP)
            MOVE "F" TO W-STATE
        WHEN "["
            ADD 1 TO P
            PERFORM PARSE-ARRAY
            MOVE "A" TO W-STATE
        WHEN OTHER
            MOVE "numbers, booleans and null are not allowed (or invalid JSON)" TO ERR-MSG
            PERFORM FAIL
    END-EVALUATE.

*> ["link", target] or ["script", content]; any other array is rejected
PARSE-ARRAY.
    MOVE "an array must be [""link"", target] or [""script"", content]" TO ERR-MSG
    PERFORM SKIP-WS
    IF W-CH NOT = '"'
        PERFORM FAIL
    END-IF
    PERFORM PARSE-STRING
    MOVE S-OFF TO K-OFF
    MOVE S-LEN TO K-LEN
    PERFORM SKIP-WS
    IF W-CH NOT = ","
        PERFORM FAIL
    END-IF
    ADD 1 TO P
    PERFORM SKIP-WS
    IF W-CH NOT = '"'
        PERFORM FAIL
    END-IF
    PERFORM PARSE-STRING
    PERFORM SKIP-WS
    IF W-CH NOT = "]"
        PERFORM FAIL
    END-IF
    ADD 1 TO P
    MOVE S-OFF TO ND-VOFF(N)
    MOVE S-LEN TO ND-VLEN(N)
    EVALUATE TRUE
        WHEN K-LEN = 4 AND POOL(K-OFF:4) = "link"
            MOVE "L" TO ND-TYPE(N)
            IF S-LEN > 0
                MOVE 0 TO CNT
                INSPECT POOL(S-OFF:S-LEN) TALLYING CNT FOR ALL X"00"
                IF CNT > 0
                    MOVE "a link target cannot contain NUL" TO ERR-MSG
                    PERFORM FAIL
                END-IF
            END-IF
        WHEN K-LEN = 6 AND POOL(K-OFF:6) = "script"
            MOVE "X" TO ND-TYPE(N)
        WHEN OTHER
            MOVE "unknown array kind" TO ERR-MSG
            PERFORM FAIL
    END-EVALUATE.

*> §4.2.1: simple names only
CHECK-NAME.
    MOVE "invalid name" TO ERR-MSG
    IF NAME-LEN = 0
        PERFORM FAIL
    END-IF
    IF NAME-LEN = 1 AND POOL(NAME-OFF:1) = "."
        PERFORM FAIL
    END-IF
    IF NAME-LEN = 2 AND POOL(NAME-OFF:2) = ".."
        PERFORM FAIL
    END-IF
    MOVE 0 TO CNT
    INSPECT POOL(NAME-OFF:NAME-LEN) TALLYING CNT FOR ALL "/" ALL X"00"
    IF CNT > 0
        PERFORM FAIL
    END-IF.

*> new child of the object on top of the stack; duplicate names are rejected (§4.2 allows it)
ADD-NODE.
    MOVE STK(SP) TO PN
    MOVE ND-FIRST(PN) TO C
    PERFORM UNTIL C = 0
        IF ND-NLEN(C) = NAME-LEN
            IF POOL(ND-NOFF(C):NAME-LEN) = POOL(NAME-OFF:NAME-LEN)
                MOVE "duplicate name" TO ERR-MSG
                PERFORM FAIL
            END-IF
        END-IF
        MOVE ND-NEXT(C) TO C
    END-PERFORM
    IF NODE-COUNT >= NODE-MAX
        MOVE "too many entries" TO ERR-MSG
        PERFORM FAIL
    END-IF
    ADD 1 TO NODE-COUNT
    MOVE NODE-COUNT TO N
    MOVE PN TO ND-PARENT(N)
    MOVE NAME-OFF TO ND-NOFF(N)
    MOVE NAME-LEN TO ND-NLEN(N)
    MOVE 0 TO ND-VOFF(N) ND-VLEN(N) ND-FIRST(N) ND-LAST(N) ND-NEXT(N) ND-PLEN(N)
    IF ND-LAST(PN) = 0
        MOVE N TO ND-FIRST(PN)
    ELSE
        MOVE N TO ND-NEXT(ND-LAST(PN))
    END-IF
    MOVE N TO ND-LAST(PN).

*> string at P (on the opening quote) -> decoded bytes in POOL(S-OFF:S-LEN)
PARSE-STRING.
    ADD 1 TO P
    COMPUTE S-OFF = POOL-LEN + 1
    PERFORM UNTIL 1 = 0
        IF P > IN-LEN
            PERFORM BAD-JSON
        END-IF
        MOVE IN-BUF(P:1) TO W-CH
        EVALUATE TRUE
            WHEN W-CH = '"'
                ADD 1 TO P
                EXIT PERFORM
            WHEN W-CH = "\"
                ADD 1 TO P
                PERFORM PARSE-ESCAPE
            WHEN FUNCTION ORD(W-CH) <= 32
                PERFORM BAD-JSON
            WHEN OTHER
                ADD 1 TO POOL-LEN
                MOVE W-CH TO POOL(POOL-LEN:1)
                ADD 1 TO P
        END-EVALUATE
    END-PERFORM
    COMPUTE S-LEN = POOL-LEN + 1 - S-OFF.

PARSE-ESCAPE.
    PERFORM GET-W-CH
    EVALUATE W-CH
        WHEN '"'
        WHEN "\"
        WHEN "/"
            MOVE FUNCTION ORD(W-CH) TO OUT-BYTE
            SUBTRACT 1 FROM OUT-BYTE
            PERFORM PUT-BYTE
        WHEN "b"
            MOVE 8 TO OUT-BYTE
            PERFORM PUT-BYTE
        WHEN "f"
            MOVE 12 TO OUT-BYTE
            PERFORM PUT-BYTE
        WHEN "n"
            MOVE 10 TO OUT-BYTE
            PERFORM PUT-BYTE
        WHEN "r"
            MOVE 13 TO OUT-BYTE
            PERFORM PUT-BYTE
        WHEN "t"
            MOVE 9 TO OUT-BYTE
            PERFORM PUT-BYTE
        WHEN "u"
            PERFORM READ-HEX4
            MOVE HX TO CP
            IF CP >= 56320 AND CP <= 57343
                PERFORM LONE-SURROGATE
            END-IF
            IF CP >= 55296 AND CP <= 56319
                IF P + 2 > IN-LEN
                    PERFORM LONE-SURROGATE
                END-IF
                IF IN-BUF(P + 1:2) NOT = "\u"
                    PERFORM LONE-SURROGATE
                END-IF
                ADD 2 TO P
                PERFORM READ-HEX4
                IF HX < 56320 OR HX > 57343
                    PERFORM LONE-SURROGATE
                END-IF
                COMPUTE CP = 65536 + (CP - 55296) * 1024 + (HX - 56320)
            END-IF
            PERFORM PUT-CODEPOINT
        WHEN OTHER
            PERFORM BAD-JSON
    END-EVALUATE
    ADD 1 TO P.

LONE-SURROGATE.
    MOVE "a string contains an unpaired surrogate" TO ERR-MSG
    PERFORM FAIL.

*> the 4 hex digits after P -> HX; P ends on the last digit
READ-HEX4.
    MOVE 0 TO HX
    PERFORM 4 TIMES
        ADD 1 TO P
        PERFORM GET-W-CH
        EVALUATE TRUE
            WHEN W-CH >= "0" AND W-CH <= "9"
                COMPUTE HX = HX * 16 + FUNCTION ORD(W-CH) - 49
            WHEN W-CH >= "a" AND W-CH <= "f"
                COMPUTE HX = HX * 16 + FUNCTION ORD(W-CH) - 88
            WHEN W-CH >= "A" AND W-CH <= "F"
                COMPUTE HX = HX * 16 + FUNCTION ORD(W-CH) - 56
            WHEN OTHER
                PERFORM BAD-JSON
        END-EVALUATE
    END-PERFORM.

PUT-BYTE.
    ADD 1 TO POOL-LEN
    MOVE FUNCTION CHAR(OUT-BYTE + 1) TO POOL(POOL-LEN:1).

PUT-CODEPOINT.
    EVALUATE TRUE
        WHEN CP < 128
            MOVE CP TO OUT-BYTE
            PERFORM PUT-BYTE
        WHEN CP < 2048
            COMPUTE T = CP / 64
            COMPUTE OUT-BYTE = 192 + T
            PERFORM PUT-BYTE
            COMPUTE OUT-BYTE = 128 + FUNCTION MOD(CP, 64)
            PERFORM PUT-BYTE
        WHEN CP < 65536
            COMPUTE T = CP / 4096
            COMPUTE OUT-BYTE = 224 + T
            PERFORM PUT-BYTE
            COMPUTE T = CP / 64
            COMPUTE OUT-BYTE = 128 + FUNCTION MOD(T, 64)
            PERFORM PUT-BYTE
            COMPUTE OUT-BYTE = 128 + FUNCTION MOD(CP, 64)
            PERFORM PUT-BYTE
        WHEN OTHER
            COMPUTE T = CP / 262144
            COMPUTE OUT-BYTE = 240 + T
            PERFORM PUT-BYTE
            COMPUTE T = CP / 4096
            COMPUTE OUT-BYTE = 128 + FUNCTION MOD(T, 64)
            PERFORM PUT-BYTE
            COMPUTE T = CP / 64
            COMPUTE OUT-BYTE = 128 + FUNCTION MOD(T, 64)
            PERFORM PUT-BYTE
            COMPUTE OUT-BYTE = 128 + FUNCTION MOD(CP, 64)
            PERFORM PUT-BYTE
    END-EVALUATE.

*> ---------------------------------------------------------------- file system
APPLY-TREE.
    *> mode for scripts: (0666 & ~umask) | 0111, one octal digit at a time
    CALL "umask" USING BY VALUE ZERO-INT RETURNING OLD-MASK
    CALL "umask" USING BY VALUE OLD-MASK RETURNING RC
    MOVE 0 TO X-MODE
    MOVE OLD-MASK TO T
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 3
        COMPUTE DIGIT = FUNCTION MOD(T, 8)
        COMPUTE T = T / 8
        MOVE 1 TO CNT
        *> bit 4 (read) and bit 2 (write) survive unless masked; bit 1 is always set
        COMPUTE MBIT = DIGIT / 4
        IF MBIT = 0
            ADD 4 TO CNT
        END-IF
        COMPUTE MBIT = FUNCTION MOD(DIGIT, 4)
        IF MBIT < 2
            ADD 2 TO CNT
        END-IF
        COMPUTE X-MODE = X-MODE + CNT * 8 ** (I - 1)
    END-PERFORM
    PERFORM VARYING N FROM 2 BY 1 UNTIL N > NODE-COUNT
        PERFORM BUILD-PATH
        PERFORM PROBE-EXISTING
        EVALUATE ND-TYPE(N)
            WHEN "O"
                PERFORM MAKE-DIR
            WHEN "L"
                PERFORM MAKE-LINK
            WHEN OTHER
                PERFORM MAKE-FILE
        END-EVALUATE
    END-PERFORM.

*> W-PATH(1:PL) = parent path "/" name, NUL-terminated; directories remember their path
BUILD-PATH.
    MOVE ND-PARENT(N) TO PN
    MOVE 0 TO PL
    IF PN NOT = 1
        MOVE PATHS(ND-POFF(PN):ND-PLEN(PN)) TO W-PATH(1:ND-PLEN(PN))
        COMPUTE PL = ND-PLEN(PN) + 1
        MOVE "/" TO W-PATH(PL:1)
    END-IF
    IF PL + ND-NLEN(N) >= PATH-MAX
        MOVE "path is too long" TO ERR-MSG
        PERFORM FAIL
    END-IF
    MOVE POOL(ND-NOFF(N):ND-NLEN(N)) TO W-PATH(PL + 1:ND-NLEN(N))
    ADD ND-NLEN(N) TO PL
    MOVE X"00" TO W-PATH(PL + 1:1)
    IF ND-TYPE(N) = "O"
        IF PATHS-LEN + PL > BUF-MAX
            MOVE "too many directories" TO ERR-MSG
            PERFORM FAIL
        END-IF
        COMPUTE ND-POFF(N) = PATHS-LEN + 1
        MOVE PL TO ND-PLEN(N)
        MOVE W-PATH(1:PL) TO PATHS(PATHS-LEN + 1:PL)
        ADD PL TO PATHS-LEN
    END-IF.

*> EXIST: "L" symlink (readlink works), "D" directory (opendir works), "?" anything else or nothing
PROBE-EXISTING.
    MOVE 4096 TO SZ
    CALL "readlink" USING BY REFERENCE W-PATH BY REFERENCE L-BUF BY VALUE SZ RETURNING RC
    IF RC >= 0
        MOVE "L" TO EXIST
    ELSE
        CALL "opendir" USING BY REFERENCE W-PATH RETURNING DIRP
        IF DIRP NOT = NULL
            CALL "closedir" USING BY VALUE DIRP RETURNING RC
            MOVE "D" TO EXIST
        ELSE
            MOVE "?" TO EXIST
        END-IF
    END-IF.

CLEAR-ENTRY.
    IF EXIST = "D"
        MOVE "a directory is in the way" TO ERR-MSG
        PERFORM FAIL-PATH
    END-IF
    CALL "unlink" USING BY REFERENCE W-PATH RETURNING RC.

MAKE-DIR.
    IF EXIST NOT = "D"
        CALL "unlink" USING BY REFERENCE W-PATH RETURNING RC
        CALL "mkdir" USING BY REFERENCE W-PATH BY VALUE D-MODE RETURNING RC
        IF RC NOT = 0
            MOVE "cannot create directory" TO ERR-MSG
            PERFORM FAIL-PATH
        END-IF
    END-IF.

MAKE-LINK.
    PERFORM CLEAR-ENTRY
    IF ND-VLEN(N) >= PATH-MAX
        MOVE "link target is too long" TO ERR-MSG
        PERFORM FAIL-PATH
    END-IF
    IF ND-VLEN(N) > 0
        MOVE POOL(ND-VOFF(N):ND-VLEN(N)) TO T-BUF(1:ND-VLEN(N))
    END-IF
    MOVE X"00" TO T-BUF(ND-VLEN(N) + 1:1)
    CALL "symlink" USING BY REFERENCE T-BUF BY REFERENCE W-PATH RETURNING RC
    IF RC NOT = 0
        MOVE "cannot create symbolic link" TO ERR-MSG
        PERFORM FAIL-PATH
    END-IF.

*> O_WRONLY|O_CREAT|O_EXCL, 0666: never writes through an entry that appeared after unlink
MAKE-FILE.
    PERFORM CLEAR-ENTRY
    CALL "open" USING BY REFERENCE W-PATH BY VALUE O-FLAGS BY VALUE O-MODE RETURNING W-FD
    IF W-FD < 0
        MOVE "cannot create file" TO ERR-MSG
        PERFORM FAIL-PATH
    END-IF
    MOVE ND-VOFF(N) TO W-OFF
    MOVE ND-VLEN(N) TO W-REM
    PERFORM UNTIL W-REM = 0
        CALL "write" USING BY VALUE W-FD BY REFERENCE POOL(W-OFF:W-REM) BY VALUE W-REM
            RETURNING RC
        IF RC <= 0
            MOVE "cannot write file" TO ERR-MSG
            PERFORM FAIL-PATH
        END-IF
        ADD RC TO W-OFF
        SUBTRACT RC FROM W-REM
    END-PERFORM
    IF ND-TYPE(N) = "X"
        CALL "fchmod" USING BY VALUE W-FD BY VALUE X-MODE RETURNING RC
        IF RC NOT = 0
            MOVE "cannot make file executable" TO ERR-MSG
            PERFORM FAIL-PATH
        END-IF
    END-IF
    CALL "close" USING BY VALUE W-FD RETURNING RC.
