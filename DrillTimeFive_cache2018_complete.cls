DrillTimeThree(pGlobal, pClass, pKeyList)
    ; Initialize error trap
    set $ZERROR = ""
    set $ZTRAP = "TRAP^DrillTimeFive"

    new keyLength, keyFields, classDef, sc
    new i, seq, propObj, propName, skip, type, coll
    new outputProps, totalProps
    new fieldNodeMap, fieldPieceMap, fieldTypeMap
    new headerLine, recCount
    new idx

    set recCount=0

    ; Use default parameters if not provided
    if $get(pKeyList)="" set pKeyList="HMO,ID,RIN"
    if $get(pGlobal)="" set pGlobal="^[""MHRREG""]HMOREF"
    if $get(pClass)="" set pClass="Referrals.Referral"

    ; Parse key list into array
    set keyLength=1
    if pKeyList'="" {
        if pKeyList?1N.N {
            set keyLength=+pKeyList
        } else {
            set keyLength=$length(pKeyList,",")
            for i=1:1:keyLength set keyFields(i)=$piece(pKeyList,",",i)
        }
    }

    ; Open class definition metadata
    set classDef=##class(%Dictionary.ClassDefinition).%OpenId(pClass,.sc)
    if '$isobject(classDef) {
        write "ERROR: Class not found or could not open: "_pClass,!
        quit
    }

    ; Get property count
    set totalProps=classDef.Properties.Count()
    if 'totalProps {
        write "No properties found in class: "_pClass,!
        quit
    }

    kill outputProps, fieldNodeMap, fieldPieceMap, fieldTypeMap

    ; Build property maps filtering out unwanted properties
    for i=1:1:totalProps {
        set propObj=classDef.Properties.GetAt(i)
        if '$isobject(propObj) continue

        set skip=0
        if propObj.Calculated set skip=1
        if propObj.Relationship set skip=1
        if propObj.MultiDimensional set skip=1
        set coll=propObj.Collection
        if coll'="" set skip=1
        set type=propObj.Type
        if type["%Stream" set skip=1

        if skip=1 continue

        set propName=propObj.Name
        set seq=propObj.SequenceNumber
        if seq="" set seq=9999

        set outputProps(seq)=propName

        set storageProps=classDef.Storages.GetAt(i)
        if $length($get(storageProps)) {
            set fieldNodeMap(propName)=storageProps.Node
            set fieldPieceMap(propName)=storageProps.Piece
        }
        set fieldTypeMap(propName)=type
    }

    ; Build CSV header line quoting property names
    set headerLine=""
    set idx=""
    for  set idx=$order(outputProps(idx)) quit:idx=""  do {
        set propName=outputProps(idx)
        if headerLine'="" {
            set headerLine=headerLine_","_""""_propName_""""
        } else {
            set headerLine=""""_propName_""""
        }
    }

    ; Store CSV header in temporary global
    set ^EXPORT($J,0)=headerLine

    ; Start recursive traversal
    do TraverseKeys(pGlobal,"",1,keyLength,.keyFields,.outputProps,.fieldNodeMap,.fieldPieceMap,.recCount)

    write "Export complete. Total records: ", recCount,!

    quit

TraverseKeys(pGlobal,partialSub,keyIndex,keyLength,keyFields,outputProps,fieldNodeMap,fieldPieceMap,recCount)
    new subKey,newPartialSub,i,propName,propNode,propPiece,value,csvValue,csvLine,lineSub

    ; When all keys collected, output CSV line for record
    if keyIndex>keyLength {
        set csvLine=""
        for i=1:1:$order(outputProps(""),-1) {
            set propName=outputProps(i)
            set propNode=$get(fieldNodeMap(propName))
            set propPiece=$get(fieldPieceMap(propName))
            if propNode'="" {
                set lineSub=partialSub
                set value=$piece($get(@(pGlobal_"("_lineSub_")")_","_propNode_")"),"^",propPiece)
            } else {
                set value=$get(@(pGlobal_"("_partialSub_")"))
            }
            ; Escape and quote for CSV
            set csvValue=value
            if csvValue["""" set csvValue=$replace(csvValue,"""","""""")
            if (csvValue[",")!(csvValue["""") set csvValue=""""_csvValue_""""
            if csvLine'="" set csvLine=csvLine_","_csvValue
            else  set csvLine=csvValue
        }
        set recCount=recCount+1
        set ^EXPORT($J,recCount)=csvLine
        quit
    }

    ; Traverse next subscript at this key position
    set subKey=""
    for  set subKey=$order(@(pGlobal_"("_partialSub_")")_","_subKey_")") quit:subKey=""  do {
        if partialSub="" {
            set newPartialSub=""""_subKey_""""
        } else {
            set newPartialSub=partialSub_","_""""_subKey_""""
        }
        do TraverseKeys(pGlobal,newPartialSub,keyIndex+1,keyLength,keyFields,outputProps,fieldNodeMap,fieldPieceMap,recCount)
    }

    quit

TRAP
    do $system.Status.DisplayError($ZERROR)
    quit
