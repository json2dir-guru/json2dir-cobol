# json2dir-cobol

json2dir in COBOL (GnuCOBOL): reads a JSON document on stdin and creates the tree it describes in the current directory.

    cobc -x -O json2dir.cob -o out/json2dir && out/json2dir < document.json
