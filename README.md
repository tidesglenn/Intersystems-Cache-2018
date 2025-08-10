this routine syntax works full with documentation
languagemode="0" timestamp="67427,55152.456236"><![CDATA[
ExportDynamic ; ExportDynamic.INT — v7.6 TEST-INSTRUMENTED (Caché 2018.1.7-safe, FK-header patch)
 ; ------------------------------------------------------------------------------
 ; PURPOSE
 ;   Test-focused build that exports mapped data from a storage global into
 ;   per-class CSV files and logs every step. Includes harness to simulate
 ;   edge cases and to run limited traversals for verification.
 ;
 ; VERSION NOTES (changes since v7.5)
 ;   - Keep $ZUTIL(5) for namespace capture per user requirement (#8).
 ;   - Full traversal: removed arity stop; always descend until no children (#2/#3).
 ;   - No $QUERY usage (#5).
 ;   - Key columns now written on each row; headers match rows (#1).
 ;   - %ID captured as SqlRowID column when mappable; no blind “head” fallback (#6/#7).
 ;   - FK extraction only if that FK property is present in dataMap/pieceSpec (#7).
 ;   - Extra WRITE instrumentation at every phase; clear error trap.
 ;   - [PATCH v7.6a] Headers only include FK columns that are actually mappable for this class
 ;                   and present in dataMap/pieceSpec for at least one mapID. Prevents empty
 ;                   trailing columns. (See BuildClassHeaders FK block below.)
 ;
 ; ENTRY POINTS
 ;   DO ExportBySchema^ExportDynamic(.parm)        ; main orchestration
 ;   DO ExportAllLevels^ExportDynamic(.parm)       ; optional full tree dump
 ;   DO TestHarness^ExportDynamic()                ; local simulation of edge cases
 ;
 ; REQUIRED PARAMS (by name in parm())
 ;   parm("Schema")     : e.g., "Referrals"
 ;   parm("GlobalRef")  : e.g., "^|""NS""|HMOREF" or "^^NS^HMOREF" or "^HMOREF"
 ;   parm("OutputDir")  : directory path (created if missing)
 ;
 ; OPTIONAL PARAMS
 ;   parm("Verbose")    : 0/1 (default 1)
 ;   parm("MaxDepth")   : traversal cap (default 200; defensive only)
 ;   parm("MaxNodes")   : node capture cap (0 = unlimited)
 ;
 ; OUTPUT
 ;   One CSV per CLASS in the schema. Columns: Key_1..Key_n, optional SqlRowID,
 ;   mapped storage properties, and raw FK ID columns when mapped in storage.
 ;
 ; ------------------------------------------------------------------------------
 ; DOC URL INDEX — Caché 2018.1.7 (verify here)
 ;   Reference index (all) ................................. https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=ALL
 ;   WRITE / strings / quoting ............................. https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_cwrite
 ;   $GET / $PIECE / $ORDER / $LENGTH / $EXTRACT ........... https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_fget
 ;                                                          https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_fpiece
 ;                                                          https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_forder
 ;                                                          https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_fextract
 ;   Pattern match (numeric test) .......................... https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_op_patternmatch
 ;   Indirection @ ......................................... https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_op_indirection
 ;   Extended global references ............................ https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GGBL_extended
 ;   Using globals ......................................... https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GGBL_using
 ;   Dynamic SQL / %SQL.Statement .......................... https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GSQL_dynsql
 ;                                                          https://docs.intersystems.com/cache2018.1/csp/documatic/%25CSP.Documatic.cls?CLASSNAME=%25SQL.Statement&LIBRARY=%25SYS
 ;   %Dictionary search .................................... https://docs.intersystems.com/cache2018.1/csp/documatic/%25CSP.Documatic.cls?LIBRARY=%25SYS&PRIVATE=1&SEARCH=%25Dictionary
 ;   %Stream.FileCharacter ................................. https://docs.intersystems.com/cache2018.1/csp/documatic/%25CSP.Documatic.cls?CLASSNAME=%25Stream.FileCharacter&LIBRARY=%25SYS
 ;
 ; SAFETY CHECKLIST (keep true)
 ;   [x] No use of Type/Collection/Relationship or multi-node props.
 ;   [x] No use of ClassDefinition.Properties array.
 ;   [x] %Dictionary fields limited to:
 ;       - StorageSQLMapSubDefinition: ID, Name
 ;       - StorageSQLMapDataDefinition: ID, Name, Piece, Delimiter
 ;       - PropertyDefinition: Parent, Name, SqlFieldName
 ;       - IndexDefinition: Parent, SqlName, IdKey
 ;       - ForeignKeyDefinition: Parent, Properties, ReferencedClass
 ;   [x] Traversal includes string, numeric, negative, decimal, and "0" subscripts.
 ;   [x] MaxDepth and MaxNodes enforced.
 ; ------------------------------------------------------------------------------
 Q
 ;
 ; ==============================================================================
 ; [ENTRY] ExportBySchema — Orchestrate full export
 ;   DOCS: Dynamic SQL, $ETRAP, extended refs
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GSQL_dynsql
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_vetrap
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GGBL_extended
 ;
ExportBySchema(parm)
    NEW $ETRAP SET $ETRAP="GOTO ERR^ExportDynamic"
    h 20
    s parm("Schema")="Referrals"
    s parm("GlobalRef")="^HMOREF"
    s parm("OutputDir")="C:\InterSystems\Cache\Mgr\Hackensack\"
    s parm("OneClass")=""
    s parm("OneClass")="Referrals.StatusRule"
    NEW Schema,GlobalRef,OutputDir,Verbose,MaxDepth,MaxNodes
    NEW CurrentClass,CurrentMapID
    SET Schema=$GET(parm("Schema"))
    SET GlobalRef=$GET(parm("GlobalRef"))
    SET OutputDir=$GET(parm("OutputDir"))
    SET Verbose=+$GET(parm("Verbose"),1)
    SET MaxDepth=+$GET(parm("MaxDepth"),200)
    SET MaxNodes=+$GET(parm("MaxNodes"),0)
    IF Schema="" WRITE !,"*** ERROR: parm(""Schema"") is required" QUIT
    IF GlobalRef="" WRITE !,"*** ERROR: parm(""GlobalRef"") is required" QUIT
    SET OutputDir=$$NormalizeDir(OutputDir)
    DO ##class(%Library.File).CreateDirectoryChain(OutputDir)
    WRITE !,"=== [ENTER] ExportBySchema ==="
    WRITE !,"[PARAM] Schema=",Schema," GlobalRef=",GlobalRef," OutputDir=",OutputDir," Verbose=",Verbose," MaxDepth=",MaxDepth," MaxNodes=",MaxNodes
    NEW rootRef SET rootRef=GlobalRef DO NormalizeGlobalRef(.rootRef,"",1)
    WRITE !,"[NS] rootRef=",rootRef
    ;
    ; Build class set for schema
    i $l($g(parm("OneClass"))){
    	n classSet s class=$g(parm("OneClass")) s classSet(class)="" s Schema=$p(class,".",1) s Verbose=1
    }else{
    	NEW classSet DO BuildClassSet(.classSet,Schema,Verbose)
    }
    IF '$DATA(classSet) WRITE !,"*** No classes found for schema: ",Schema QUIT
    WRITE !,"[INFO] Classes=",$$CountParents(.classSet)
    ;
    ; Load storage maps for this schema only (filter via LIKE 'Schema.%')
    NEW subMap,dataMap
    DO LoadStorageMeta(.subMap,.dataMap,Schema,Verbose)
    WRITE !,"[INFO] SubMap IDs=",$$CountParents(.subMap)," DataMap IDs=",$$CountParents(.dataMap)
    ;
    ; Compute pieceSpec and pieceOrd from dataMap
    NEW pieceSpec,pieceOrd DO BuildPieceMap(.dataMap,.pieceSpec,.pieceOrd,Verbose)
    ;
    ; Filter map IDs to classes actually present
    NEW tableSet DO BuildTableSet(.tableSet,.classSet,.subMap,Verbose,.dataMap)
    WRITE !,"[INFO] TableSet entries=",$$CountParents(.tableSet)
    ;
    ; Build property names, IdKey names, and FK maps (Properties CSV, safe fields only)
    NEW propMap,fkMap,idKeyMap DO BuildPropertyMap(.propMap,.fkMap,.idKeyMap,.classSet,Verbose)
    ;
    ;h 20
    ; Traverse and collect rows per class (full traversal, no arity stop)
    NEW tableRows SET tableRows(0)=0
    NEW className SET className=""
    FOR  SET className=$ORDER(tableSet(className)) QUIT:className=""  DO
    . WRITE !,"[CLASS] ",className
    . i $l($g(parm("OneClass"))) q:className'=$g(parm("OneClass"))
    . NEW id SET id=""
    . FOR  SET id=$ORDER(tableSet(className,id)) QUIT:id=""  DO
    . . WRITE !,"  [MAPID] ",id
    . . NEW subsSpec SET subsSpec=""
    . . FOR  SET subsSpec=$ORDER(subMap(id,subsSpec)) QUIT:subsSpec=""  DO
    . . . WRITE !,"    [SUBS] ",subsSpec
    . . . NEW base SET base=$$MakeBase(rootRef)
    . . . NEW outcnt SET outcnt=0
    . . . SET CurrentClass=className,CurrentMapID=id
    . . . ; NOTE: subsCount is kept for logs only; traversal does not stop by arity
    . . . NEW arity SET arity=$L(subsSpec,",")
    . . . DO ReadGlobal(base,"",0,arity,.tableRows,.dataMap,.pieceSpec,.pieceOrd,.fkMap,.idKeyMap,.outcnt,MaxNodes,Verbose,MaxDepth)
    . . . WRITE !,"    [DONE] rows added so far=",+$GET(tableRows(0))
    ;
    ; Write per-class CSVs
    WRITE !,"[WRITE] emitting CSVs to: ",OutputDir
    ;h 20
    DO WriteAllCSVs(.tableSet,.pieceSpec,.pieceOrd,.propMap,.fkMap,.idKeyMap,OutputDir,Verbose,.tableRows)
    WRITE !,"=== [DONE] ExportBySchema ==="
    QUIT
 ;
 ; ==============================================================================
 ; [ERROR] ERR — Global error trap
 ;   DOCS: $ZERROR
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_vzerror
 ;
ERR NEW err SET err=$ZERROR WRITE !,"*** UNEXPECTED ERROR: ",err,! SET $ECODE="" QUIT
 ;
 ; ==============================================================================
 ; Helpers
 ;
 ; [HELPER] RSGetData / RSGet — Safe result getters
 ;   DOCS: %SQL.StatementResult.%GetData / %Get
 ;     https://docs.intersystems.com/cache2018.1/csp/documatic/%25CSP.Documatic.cls?CLASSNAME=%25SQL.Statement&LIBRARY=%25SYS
RSGetData(rs,n) NEW v SET v="" TRY { SET v=rs.%GetData(n) } CATCH ex {} QUIT v
RSGet(rs,name)  NEW v SET v="" TRY { SET v=rs.%Get(name)   } CATCH ex {} QUIT v
 ;
 ; [HELPER] Log — gated by 1/0
Log(v,msg) IF +$GET(v) WRITE !,msg QUIT
 ;
 ; [HELPER] NormalizeDir — ensure trailing slash
 ;   DOCS: $EXTRACT
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_fextract
NormalizeDir(dir)
    NEW d SET d=$GET(dir) IF d="" QUIT ""
    NEW l SET l=$L(d)
    IF $EXTRACT(d,l)'="\",$EXTRACT(d,l)'="/" SET d=d_"\\"
    QUIT d
 ;
 ; [HELPER] NormalizeGlobalRef — to ^|"NS"|Global (accept ^^NS^Global and ^Global)
 ;   DOCS: Extended refs
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GGBL_extended
NormalizeGlobalRef(rootRef,ns,Verbose)
    NEW g SET g=$GET(rootRef) IF g="" QUIT
    IF $EXTRACT(g,1,2)="^^" DO  DO Log(1,"[NS] ^^ form -> "_rootRef) QUIT
    . NEW rest SET rest=$EXTRACT(g,3,$L(g))
    . NEW nsn SET nsn=$PIECE(rest,"^",1)
    . NEW glb SET glb=$PIECE(rest,"^",2,999)
    . SET rootRef="^|"""_nsn_"""|"_$SELECT(glb'="":glb,1:$PIECE(rest,"^",2))
    IF $EXTRACT(g,1)="^",g'?1"^|".E DO
    . NEW curNS SET curNS=$ZUTIL(5)  ; per user preference
    . SET rootRef="^|"""_curNS_"""|"_$EXTRACT(g,2,$L(g))
    DO Log(1,"[NS] normalized="_rootRef)
    QUIT
 ;
 ; ==============================================================================
 ; [DICT] BuildClassSet — classes in schema (Parent = class name)
 ;   DOCS: %Dictionary.ClassDefinition.Name, %SQL.Statement
BuildClassSet(classSet,Schema,Verbose)
    DO Log(Verbose,"--- [ENTER] BuildClassSet ---")
    K classSet NEW sc,stmt,rs,sql,prefix SET prefix=Schema_"."
    SET sql="SELECT Name FROM %Dictionary.ClassDefinition WHERE Name LIKE '"_prefix_"%'"
    SET stmt=##class(%SQL.Statement).%New()
    TRY {
        SET sc=stmt.%Prepare(sql) DO Log(Verbose,"[SQL] Prepare="_sc) QUIT:$SYSTEM.Status.IsError(sc)
        SET rs=stmt.%Execute() DO Log(Verbose,"[SQL] Execute ok")
        FOR  QUIT:'rs.%Next()  DO
        . NEW name SET name=$$RSGet(rs,"Name") QUIT:name=""
        . IF $L(name,".")=2 SET classSet(name)=""
    } CATCH ex { DO Log(1,"*** ERROR: BuildClassSet: "_$ZERROR) }
    DO Log(Verbose,"[DICT] classes found="_$$CountParents(.classSet))
    QUIT
 ;
 ; ==============================================================================
 ; [DICT] LoadStorageMeta — subscript specs + data piece rules for this schema
 ;   DOCS: %Dictionary.StorageSQLMap(Sub|Data)Definition safe fields
LoadStorageMeta(subMap,dataMap,Schema,Verbose)
    K subMap,dataMap NEW sc,stmt,rs,sql,cntS,cntP SET cntS=0,cntP=0
    DO Log(Verbose,"--- [ENTER] LoadStorageMeta ---")
    SET sql="SELECT ID,Name FROM %Dictionary.StorageSQLMapSubDefinition WHERE ID LIKE '"_Schema_".%' ORDER BY ID,Name"
    SET stmt=##class(%SQL.Statement).%New()
    SET sc=stmt.%Prepare(sql) DO Log(Verbose,"[SubDef] Prepare="_sc) IF $SYSTEM.Status.IsError(sc) QUIT
    SET rs=stmt.%Execute()
    FOR  QUIT:'rs.%Next()  DO
    . NEW id SET id=$$RSGetData(rs,1) QUIT:id=""
    . NEW name SET name=$$RSGetData(rs,2) QUIT:name=""
    . NEW base SET base=$PIECE(id,"||",1,3)
    . SET subMap(base,name)=id,cntS=cntS+1
    DO Log(Verbose,"[INFO] SubDef rows="_cntS)
    SET sql="SELECT ID,Name,Piece,Delimiter FROM %Dictionary.StorageSQLMapDataDefinition WHERE ID LIKE '"_Schema_".%' ORDER BY ID,Name"
    SET stmt=##class(%SQL.Statement).%New()
    SET sc=stmt.%Prepare(sql) DO Log(Verbose,"[DataDef] Prepare="_sc) IF $SYSTEM.Status.IsError(sc) QUIT
    SET rs=stmt.%Execute()
    FOR  QUIT:'rs.%Next()  DO
    . NEW id SET id=$$RSGetData(rs,1) QUIT:id=""
    . NEW name SET name=$$RSGetData(rs,2) QUIT:name=""
    . NEW piece SET piece=$$RSGetData(rs,3)
    . NEW delim SET delim=$$RSGetData(rs,4)
    . s fullID=""
    . s fullID=id
    . q:'$l($g(fullID))
    . set id=$p(id,"||",1,3)
    . SET dataMap(id,name)=piece_"|"_delim_"|"_$g(fullID)
    . s cntP=cntP+1
    DO Log(Verbose,"[INFO] DataDef rows="_cntP)
    QUIT
 ;
 ; ==============================================================================
 ; [MAP] BuildPieceMap — compute pieceSpec and pieceOrd
 ;   Fix: parse Piece once; do not assume secondary delimiter exists here.
 ;   Notes: p="n" or "n,m". p2 stays 0 when absent. No delimiter defaults here.
 ; ==============================================================================
BuildPieceMap(dataMap,pieceSpec,pieceOrd,Verbose)
    DO Log(Verbose,"--- [ENTER] BuildPieceMap ---")
    K pieceSpec,pieceOrd
    NEW id,name,raw,p,p1,p2
    SET id=""
    FOR  SET id=$ORDER(dataMap(id)) QUIT:id=""  DO
    . SET name=""
    . FOR  SET name=$ORDER(dataMap(id,name)) QUIT:name=""  DO
    . . SET raw=$GET(dataMap(id,name)) QUIT:raw=""
    . . SET p=$PIECE(raw,"|",1)
    . . SET p1=+$PIECE(p,",",1)
    . . SET p2=+$PIECE(p,",",2)  ; 0 if absent
    . . IF 'p1 SET p1=1
    . . SET pieceSpec(id,name,1)=p1
    . . SET pieceSpec(id,name,2)=p2
    . . SET pieceOrd(id,$$ZFill(p1,5)_"_"_name)=""
    DO Log(Verbose,"[DICT] pieceSpec/pieceOrd ready")
    QUIT

 ;
 ; ==============================================================================
 ; [MAP] BuildTableSet — filter map IDs to selected classes
 ;   DOCS: $ORDER, $DATA
BuildTableSet(tableSet,classSet,subMap,Verbose,dataMap)
	; switch data map and sub
    DO Log(Verbose,"--- [ENTER] BuildTableSet ---")
    K tableSet 
    NEW id ,cls
    SET cls=""
    FOR { SET cls=$ORDER(classSet(cls)) QUIT:cls=""  
    	set id=""
    	f { set id=$o(subMap(id)) q:id=""  
    		NEW cls SET cls=$PIECE(id,"||",1)
   			continue:'$l($g(id))
    		continue:'$l($g(cls))
    		IF $DATA(dataMap(id)) SET tableSet(cls,id)=id
    	}
    }
    DO Log(Verbose,"[DICT] tableSet entries="_$$CountParents(.tableSet))
    QUIT
 ;
 ; ==============================================================================
 ; [MAP] BuildPropertyMap — property names, IdKey column, FK map (Properties list)
 ;   DOCS: %Dictionary.PropertyDefinition / IndexDefinition /
 ;         %Dictionary.ForeignKeyDefinition (Properties, ReferencedClass)
BuildPropertyMap(propMap,fkMap,idKeyMap,classSet,Verbose)
    DO Log(Verbose,"--- [ENTER] BuildPropertyMap ---")
    K propMap,fkMap,idKeyMap
    NEW stmt,rs,sc
    ; Property display names
    SET stmt=##class(%SQL.Statement).%New()
    SET sc=stmt.%Prepare("SELECT Parent,Name,SqlFieldName FROM %Dictionary.PropertyDefinition ORDER BY Parent,Name")
    IF '$SYSTEM.Status.IsError(sc) DO
    . SET rs=stmt.%Execute()
    . FOR  QUIT:'rs.%Next()  DO
    . . NEW par SET par=$$RSGet(rs,"Parent") QUIT:par=""
    . . IF '$DATA(classSet(par)) QUIT
    . . NEW nm SET nm=$$RSGet(rs,"Name")
    . . NEW sfn SET sfn=$$RSGet(rs,"SqlFieldName") IF sfn="" SET sfn=nm
    . . SET propMap(par,nm)=sfn
    ; IdKey column name
    SET stmt=##class(%SQL.Statement).%New()
    SET sc=stmt.%Prepare("SELECT Parent,SqlName,Name,IdKey FROM %Dictionary.IndexDefinition WHERE IdKey=1")
    IF '$SYSTEM.Status.IsError(sc) DO
    . SET rs=stmt.%Execute()
    . FOR  QUIT:'rs.%Next()  DO
    . . NEW par SET par=$$RSGet(rs,"Parent") QUIT:par=""
    . . IF '$DATA(classSet(par)) QUIT
    . . NEW nm SET nm=$$RSGet(rs,"SqlName") IF nm'="" SET idKeyMap(par)=nm
    . . i '$l($g(nm)) s nm=$$RSGet(rs,"Name") if nm'="" s idKeyMap(par)=nm
    ; Foreign keys: Properties CSV, only register FK props we can actually map later
    SET stmt=##class(%SQL.Statement).%New()
    SET sc=stmt.%Prepare("SELECT Parent,Properties,ReferencedClass FROM %Dictionary.ForeignKeyDefinition ORDER BY Parent,Properties")
    IF '$SYSTEM.Status.IsError(sc) DO
    . SET rs=stmt.%Execute()
    . FOR  QUIT:'rs.%Next()  DO
    . . NEW par SET par=$$RSGet(rs,"Parent") QUIT:par=""
    . . IF '$DATA(classSet(par)) QUIT
    . . s props="" SET props=$$RSGet(rs,"Properties")
    . . NEW rc SET rc=$$RSGet(rs,"ReferencedClass")
    . . IF +Verbose WRITE !,"[FK] ",par," props=",props," -> ",rc
    . . NEW i FOR i=1:1:$L(props,",") DO
    . . . NEW p SET p=$ZSTRIP($PIECE(props,",",i),"<W") QUIT:p=""
    . . . ; Registration deferred: actual extraction requires dataMap/pieceSpec in BuildRow
    . . . SET fkMap(par,p)=rc
    DO Log(Verbose,"[DICT] propMap="_$$CountParents(.propMap)_" idKeyMap="_$$CountParents(.idKeyMap)_" fkMap="_$$CountParents(.fkMap))
    QUIT
 ;
 ; ==============================================================================
 ; Traversal with capture at every depth. Supports "0", strings, numerics, decimals.
 ; base must end with "(" and no closing ")". subsList is comma-separated list.
 ;   DOCS: Using globals, $ORDER, indirection
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GGBL_using
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_forder
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_op_indirection
 ;
ReadGlobal(base,subsList,depth,subsCount,tableRows,dataMap,pieceSpec,pieceOrd,fkMap,idKeyMap,cnt,maxnodes,Verbose,MaxDepth)
    IF depth>+MaxDepth QUIT
    IF +maxnodes,+(+cnt)>=+maxnodes QUIT
    IF Verbose WRITE !,"[RG] depth=",depth," have=",$S(subsList="":0,1:$L(subsList,","))," subs='",subsList,"' base='",base,"'"
    ; Capture value at current node if any
    NEW val 
    ;h 20
    w !,"BASE="_$g(base)
    i '$l($g(subsList)){
	     s val=""
    }else{
	    s val=$g(@(base_")"))
    }
    IF val'="" DO
    . w !,$g(val)
    . IF Verbose WRITE !,"[RG] VALUE depth=",depth," len=",$L(val)," subs='",subsList,"'"
    . DO RecordNode(subsList,val,.tableRows,.dataMap,.pieceSpec,.pieceOrd,.fkMap,.idKeyMap,Verbose)
    . SET cnt=$GET(cnt)+1
    ; Enumerate children starting from "" (supports subscript 0 and strings)
    NEW sep SET sep=$SELECT($EXTRACT(base,$L(base))="(":"",1:",")
    NEW childSub SET childSub=$ORDER(@(base_sep_""""_""""_")"))
    IF Verbose WRITE !,"[RG] first child=",$SELECT(childSub="":"<none>",1:childSub)
    FOR  QUIT:childSub=""  DO
    . NEW qChild,nextBase,nextList
    . SET qChild=$REPLACE(childSub,"""","""""")
    . IF childSub=+childSub SET nextBase=base_sep_childSub,nextList=$S(subsList="":childSub,1:subsList_","_childSub)
    . ELSE  SET nextBase=base_sep_""""_qChild_"""",nextList=$S(subsList="":childSub,1:subsList_","_childSub)
    . DO ReadGlobal(nextBase,nextList,depth+1,subsCount,.tableRows,.dataMap,.pieceSpec,.pieceOrd,.fkMap,.idKeyMap,.cnt,maxnodes,Verbose,MaxDepth)
    . IF childSub=+childSub SET childSub=$ORDER(@(base_sep_childSub_")"))
    . ELSE  SET childSub=$ORDER(@(base_sep_""""_qChild_""""_")"))
    QUIT
 ;
 ; Build a row from node value using pieceSpec + dataMap; include FK raw IDs when mapped
 ;
RecordNode(subsList,val,tableRows,dataMap,pieceSpec,pieceOrd,fkMap,idKeyMap,Verbose)
NEW className SET className=$GET(CurrentClass) QUIT:className=""
NEW mapId SET mapId=$GET(CurrentMapID) QUIT:mapId=""
NEW keyCSV SET keyCSV=""
NEW i FOR i=1:1:$L(subsList,",") SET keyCSV=keyCSV_$$CSVQ($PIECE(subsList,",",i))_","
SET keyCSV=$SELECT(keyCSV'="":$EXTRACT(keyCSV,1,$L(keyCSV)-1),1:"")
NEW node K node SET node(1,"@")=val
NEW row K row DO BuildRow(.row,className,mapId,.node,.dataMap,.pieceSpec,.pieceOrd,.fkMap,.idKeyMap,Verbose)
IF $DATA(row) DO
. ; optional debug snapshot
. NEW debugPayload SET debugPayload=$$JoinCSV(.row)


 ; Header/row helpers for label alignment and key handling
RawForPretty(parent,pretty,propMap)
    NEW nm SET nm="" NEW hit SET hit=""
    FOR  SET nm=$ORDER(propMap(parent,nm)) QUIT:nm=""  DO  QUIT:$GET(hit)'=""
    . IF $GET(propMap(parent,nm))=pretty SET hit=nm
    QUIT $GET(hit)

IsKeyLabel(lab,parm)
    NEW k FOR k=1:1:32 DO  QUIT:$GET(%done)
    . IF lab=$GET(parm("KeyName",k),"Key_"_k) SET %done=1
    QUIT +$GET(%done)

KeyIndexForLabel(lab,parm)
    NEW k FOR k=1:1:32 DO  QUIT:$GET(%done)
    . IF lab=$GET(parm("KeyName",k),"Key_"_k) SET %val=k,%done=1
    QUIT +$GET(%val)

. WRITE !,"[RN] row map captured for ",className," ",mapId," keys=",keyCSV," snapshot=",debugPayload
. ; store as label->value map for header-aligned emission
. K tableRows(className,mapId,keyCSV)
. MERGE tableRows(className,mapId,keyCSV,"@row")=row
. SET tableRows(0)=$GET(tableRows(0))+1
QUIT
BuildRow(row,parent,id,node,dataMap,pieceSpec,pieceOrd,fkMap,idKeyMap,Verbose)
DO Log(Verbose,"        [ENTER] BuildRow parent="_parent_" id="_id)
K row NEW head SET head=$GET(node(1,"@")) IF head="" QUIT

; %ID -> SqlRowID column (only if mappable)
IF $GET(idKeyMap(parent))'="" DO
. NEW meta SET meta=$GET(dataMap(id,"%ID")) QUIT:meta=""
. NEW dspec SET dspec=$PIECE(meta,"|",2)  ; may be "" -> handled in BRSplit2
. NEW p1 SET p1=+$GET(pieceSpec(id,"%ID",1)) IF 'p1 SET p1=1
. NEW p2 SET p2=+$GET(pieceSpec(id,"%ID",2))
. NEW SqlRowID SET SqlRowID=$$BRSplit2(dspec,head,p1,p2)
. SET row("%SqlRowID")=$$CSVQ(SqlRowID)

; mapped storage properties in storage order
NEW ord SET ord=""
FOR  SET ord=$ORDER(pieceOrd(id,ord)) QUIT:ord=""  DO
. NEW name SET name=$PIECE(ord,"_",2) IF name="%ID" QUIT
. NEW p1 SET p1=+$GET(pieceSpec(id,name,1)) IF 'p1 SET p1=1
. NEW p2 SET p2=+$GET(pieceSpec(id,name,2))
. NEW dspec SET dspec=$PIECE($GET(dataMap(id,name)),"|",2)
. NEW v SET v=$$BRSplit2(dspec,head,p1,p2)
. SET row(name)=$$CSVQ(v)

; FK raw IDs only when FK property is actually mappable
NEW fkprop SET fkprop=""
FOR  SET fkprop=$ORDER(fkMap(parent,fkprop)) QUIT:fkprop=""  DO
. NEW spec SET spec=$GET(dataMap(id,fkprop)) QUIT:spec=""
. NEW fp1 SET fp1=+$GET(pieceSpec(id,fkprop,1)) IF 'fp1 SET fp1=1
. NEW fp2 SET fp2=+$GET(pieceSpec(id,fkprop,2))
. NEW dspec SET dspec=$PIECE(spec,"|",2)
. NEW fv SET fv=$$BRSplit2(dspec,head,fp1,fp2)
. SET row(fkprop)=$$CSVQ(fv)
QUIT
WriteAllCSVs(tableSet,pieceSpec,pieceOrd,propMap,fkMap,idKeyMap,OutputDir,Verbose,tableRows)
    DO Log(Verbose,"--- [ENTER] WriteAllCSVs ---")
    NEW className SET className=""
    FOR  SET className=$ORDER(tableRows(className)) QUIT:className=""  DO
    . q:className?1N.N
    . i $l($g(parm("OneClass"))) q:$g(parm("OneClass"))'=className
    . NEW filename SET filename=$$FileForClass(OutputDir,className) w !,$g(filename)
    . DO WriteOneClassCSV(className,.tableSet,.pieceOrd,.propMap,.fkMap,.idKeyMap,filename,Verbose,.tableRows)
    QUIT
 ;
WriteOneClassCSV(parent,tableSet,pieceOrd,propMap,fkMap,idKeyMap,filename,Verbose,tableRows)
IF parent="" QUIT
DO Log(Verbose,"[FILE] -> "_filename)
NEW s SET s=##class(%Stream.FileCharacter).%New()
NEW sc SET sc=s.LinkToFile(filename) IF $SYSTEM.Status.IsError(sc) DO $SYSTEM.Status.DisplayError(sc) QUIT
NEW headers DO BuildClassHeaders(.headers,parent,.tableRows,.pieceOrd,.propMap,.fkMap,.idKeyMap,Verbose)
DO WriteLine(s,$$JoinCSV(.headers))
NEW id SET id=""
FOR  SET id=$ORDER(tableRows(parent,id)) QUIT:id=""  DO
. NEW subs SET subs=""
. FOR  SET subs=$ORDER(tableRows(parent,id,subs)) QUIT:subs=""  DO
. . NEW i,lab,val K line
. . ; split key subs into tokens
. . NEW n SET n=$S(subs="":0,1:$L(subs,","))
. . NEW t K t NEW k FOR k=1:1:n SET t(k)=$PIECE(subs,",",k)
. . ; emit values in finalized header order
. . SET i=""
. . FOR  SET i=$ORDER(headers(i)) QUIT:i=""  DO
. . . SET lab=$GET(headers(i)) QUIT:lab=""
. . . ; Keys (rename-aware)
. . . IF $$IsKeyLabel(lab,.parm) DO  SET line(i)=$$CSVQ($GET(t($$KeyIndexForLabel(lab,.parm)))) QUIT
. . . ; UniqueID
. . . IF lab="UniqueID" DO  SET line(i)=$$CSVQ($GET(t(1))_$SELECT(n>1:"|"_$GET(t(2)),1:"")_$SELECT(n>2:"|"_$GET(t(3)),1:"")_$SELECT(n>3:"|"_$GET(t(4)),1:"")_$SELECT(n>4:"|"_$GET(t(5)),1:"")) QUIT
. . . ; SqlRowID column
. . . IF $GET(idKeyMap(parent))'="",lab=$GET(idKeyMap(parent)) DO  SET line(i)=$GET(tableRows(parent,id,subs,"@row","%SqlRowID")) QUIT
. . . ; Properties by pretty label -> raw name
. . . NEW raw SET raw=$$RawForPretty(parent,lab,.propMap)
. . . IF raw'="" SET val=$GET(tableRows(parent,id,subs,"@row",raw)) SET line(i)=$GET(val) QUIT
. . . ; FK columns were added by raw prop name, so lab equals the raw name
. . . SET line(i)=$GET(tableRows(parent,id,subs,"@row",lab))
. . NEW out SET out=$$JoinCSV(.line)
. . DO WriteLine(s,out)
SET sc=s.%Save() IF $SYSTEM.Status.IsError(sc) DO $SYSTEM.Status.DisplayError(sc)
DO s.%Close()
QUIT
BuildClassHeaders(headers,parent,tableRows,pieceOrd,propMap,fkMap,idKeyMap,Verbose)
K headers NEW i SET i=1
; Detect max keys for this parent by inspecting captured subs
NEW maxk SET maxk=1
NEW id SET id=""
FOR  SET id=$ORDER(tableRows(parent,id)) QUIT:id=""  DO
. NEW subs SET subs=""
. FOR  SET subs=$ORDER(tableRows(parent,id,subs)) QUIT:subs=""  DO
. . NEW n SET n=$S(subs="":1,1:$L(subs,",")) IF n>maxk SET maxk=n
; Key labels with optional rename support
NEW k FOR k=1:1:maxk DO
. NEW old SET old="Key_"_k
. NEW new SET new=$GET(parm("KeyName",k),old)
. SET headers(i)=new,i=i+1
; Optional SqlRowID
IF $GET(idKeyMap(parent))'="" SET headers(i)=idKeyMap(parent),i=i+1
; Properties by storage order, de-dup by PRINTED label (pretty)
NEW seen SET seen=""
NEW id2 SET id2=""
FOR  SET id2=$ORDER(pieceOrd(id2)) QUIT:id2=""  DO
. IF $PIECE(id2,"||",1)'=parent QUIT
. NEW ord SET ord=""
. FOR  SET ord=$ORDER(pieceOrd(id2,ord)) QUIT:ord=""  DO
. . NEW name SET name=$PIECE(ord,"_",2) IF name="%ID" QUIT
. . NEW pretty SET pretty=$GET(propMap(parent,name)) IF pretty="" SET pretty=name
. . IF $DATA(seen(pretty)) QUIT  SET seen(pretty)=""
. . SET headers(i)=pretty,i=i+1
; FK ID columns only when extractable
NEW fkprop SET fkprop=""
FOR  SET fkprop=$ORDER(fkMap(parent,fkprop)) QUIT:fkprop=""  DO
. NEW hasMap SET hasMap=0
. NEW chkId SET chkId=""
. FOR  SET chkId=$ORDER(pieceOrd(chkId)) QUIT:chkId=""  DO
. . IF $PIECE(chkId,"||",1)'=parent QUIT
. . IF $GET(dataMap(chkId,fkprop))'="",$GET(pieceSpec(chkId,fkprop,1))'="" SET hasMap=1 QUIT
. IF hasMap,'$DATA(seen(fkprop)) SET seen(fkprop)="",headers(i)=fkprop,i=i+1
; Append UniqueID
SET headers(i)="UniqueID",i=i+1
DO Log(Verbose,"[HEAD] cols="_(i-1)_" keys="_(maxk))
QUIT
WriteLine(stream,text) DO stream.Write(text),stream.Write($CHAR(13,10)) QUIT
CSVQ(x) 
	NEW s 
	SET s=$GET(x) 
	IF ((s[",")||(s["""")||(s[$CHAR(10))||(s[$CHAR(13))) SET s=""""_$REPLACE(s,"""","""""")_"""" 
	QUIT s
JoinCSV(arr) 
	NEW out 
	SET out="" 
	NEW j 
	SET j=""
	FOR { SET j=$ORDER(arr(j)) QUIT:j=""  
		SET out=out_$GET(arr(j))_"," 
	}
	
	QUIT $EXTRACT(out,1,$L(out)-1)
JoinCSVKeys(subs) 
	NEW out 
	SET out="" 
	NEW i 
	FOR i=1:1:$L($GET(subs),","){ 
		SET out=out_$$CSVQ($PIECE(subs,",",i))_"," 
	} 
	QUIT $SELECT(out'="":$EXTRACT(out,1,$L(out)-1),1:"")
 ;
 ; File helpers
FileForClass(dir,parent) 
	NEW safe 
	SET safe=$ZCONVERT(parent,"O","JSON") 
	QUIT $$NormalizeDir(dir)_safe_".csv"
FileForAll(dir,rootRef) 
	NEW g 
	SET g=rootRef 
	IF (g?1"^|".E){ SET g=$PIECE(g,"|",3,99) }
	IF g?1"^".E SET g=$EXTRACT(g,2,$L(g)) 
	NEW safe 
	SET safe=$TRANSLATE(g,":/\\()"" ","_____") 
	QUIT $$NormalizeDir(dir)_safe_"__ALL.csv"
 ;
 ; Util
CountParents(arr) 
	NEW n 
	SET n=0 
	NEW k 
	SET k=""
	FOR { SET k=$ORDER(arr(k)) QUIT:k=""  
	 	SET n=n+1 
	 } 
	QUIT n
ZFill(n,w) 
	NEW s 
	SET s=+$GET(n) 
	NEW width 
	SET width=+$GET(w) 
	IF (width<1){ SET width=1} 
	NEW pad 
	SET pad=$EXTRACT($TRANSLATE($JUSTIFY("",width)," ","0")_$GET(s),$L($GET(s))+1,width+$L($GET(s))) 
	QUIT pad
	
DetectMaxKeysForParent(parent) 
	NEW m SET m=1 
 	QUIT m  ; unused (kept for compatibility)
 ;
 ; ==============================================================================
 ; Diagnostics — Full traversal dump (optional)
 ;   DOCS: Using globals, indirection
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=GGBL_using
 ;     https://docs.intersystems.com/cache2018.1/csp/docbook/DocBook.UI.Page.cls?KEY=RCOS_op_indirection
 ;
ExportAllLevels(parm)
    NEW $ETRAP SET $ETRAP="GOTO ERR^ExportDynamic"
    NEW GlobalRef,OutputDir,Verbose,MaxDepth,MaxNodes
    SET GlobalRef=$GET(parm("GlobalRef"))
    SET OutputDir=$GET(parm("OutputDir"))
    SET Verbose=+$GET(parm("Verbose"),1)
    SET MaxDepth=+$GET(parm("MaxDepth"),200)
    SET MaxNodes=+$GET(parm("MaxNodes"),0)
    IF GlobalRef="" WRITE !,"*** ERROR: parm(""GlobalRef"") is required" QUIT
    SET OutputDir=$$NormalizeDir(OutputDir)
    DO ##class(%Library.File).CreateDirectoryChain(OutputDir)
    NEW rootRef SET rootRef=GlobalRef DO NormalizeGlobalRef(.rootRef,"",1)
    NEW base SET base=$$MakeBase(rootRef)
    NEW out SET out=$$FileForAll(OutputDir,rootRef)
    NEW s SET s=##class(%Stream.FileCharacter).%New()
    NEW sc SET sc=s.LinkToFile(out) IF $SYSTEM.Status.IsError(sc) DO $SYSTEM.Status.DisplayError(sc) QUIT
    ;SET sc=s.CreateNew() IF $SYSTEM.Status.IsError(sc) DO $SYSTEM.Status.DisplayError(sc) QUIT
    DO WriteLine(s,"Level,Sub1,Sub2,Sub3,Sub4,Sub5,Sub6,Sub7,Sub8,Sub9,Sub10,Value")
    NEW cnt SET cnt=0
    DO DumpAll(.s,base,0,MaxDepth,.cnt,MaxNodes,Verbose)
    SET sc=s.%Save() IF $SYSTEM.Status.IsError(sc) DO $SYSTEM.Status.DisplayError(sc)
    DO s.%Close()
    WRITE !,"=== [DONE] Dumped ",cnt," nodes -> ",out," ==="
    QUIT
	
 ;
DumpAll(s,base,depth,maxdepth,cnt,maxnodes,Verbose)
    IF depth>maxdepth QUIT
    IF +maxnodes,+(+cnt)>=+maxnodes QUIT
    NEW val SET val=$GET(@(base_")"))
    IF val'="" DO
    . NEW row K row SET row(1)=depth
    . NEW i FOR i=1:1:10 SET row(1+i)=$$CSVQ($$SubAt(base,i))
    . SET row(12)=$$CSVQ(val)
    . DO WriteLine(s,$$JoinCSV(.row))
    . SET cnt=$GET(cnt)+1 IF +cnt#100=0 DO Log(Verbose,"[DUMP] rows="_cnt)
    NEW childSub SET childSub=$ORDER(@(base_","_""""))
    FOR  QUIT:childSub=""  DO
    . NEW nextBase SET nextBase=$S(childSub=+childSub:base_","_childSub,1:base_","""_$$Q(childSub)_"""")
    . DO DumpAll(.s,nextBase,depth+1,maxdepth,.cnt,maxnodes,Verbose)
    . SET childSub=$S(childSub=+childSub:$ORDER(@(base_","_childSub)),1:$ORDER(@(base_","""_$$Q(childSub)_"""")))
    QUIT
 ;
SubAt(base,i) NEW arg SET arg=$PIECE(base,"(",2) NEW n SET n=$L(arg,",") IF i>n QUIT "" NEW p SET p=$PIECE(arg,",",i) IF p=+p QUIT p IF $EXTRACT(p)=$CHAR(34),$EXTRACT(p,$L(p))=$CHAR(34) SET p=$EXTRACT(p,2,$L(p)-1) QUIT $REPLACE(p,"""","""""")
MakeBase(ref) 
	NEW b 
	SET b=$GET(ref) 
	IF b="" QUIT $g(b)
	IF ($FIND(b,"(")=0) { 
		SET b=b_"(" 
		q b
	} 
	i $EXTRACT(b,$L(b))=")" {
			SET b=$EXTRACT(b,1,$L(b)-1) quit b
	}
	QUIT b
	
Q(x) QUIT $ZCONVERT($ZSTRIP($GET(x),"<>W"),"O","JSON")
 ;
 ; ==============================================================================
 ; [TEST] TestHarness — build a tiny synthetic tree covering edge cases and export
 ;   Cases: ^TEST(0)=..., numeric child, decimal child, string child with quotes
 ;   Use:  DO TestHarness^ExportDynamic
 ;   Then: DO ExportAllLevels^ExportDynamic(.p) or ExportBySchema with your schema
 ;
TestHarness()
    NEW $ETRAP SET $ETRAP="GOTO ERR^ExportDynamic"
    NEW g SET g="^||TEST(""EDGE"")" K @g
    ; Populate edge nodes
    SET @g@(0)="^1"           ; header-like with empty first piece
    SET @g@(1)="0^3"          ; header with count form
    SET @g@(2,640833,"SCHED",0)="^1"
    SET @g@(2,651760,"SCHED",0)="0^1"
    SET @g@(2,651760,"SCHED",2052414)="63505^36000"
    SET @g@(2,657770,"BAR","A")="foo^bar^baz"
    SET @g@(2,657770,"HIST",0)="1^1"
    SET @g@(2,657770,"NAME","Last, ""O'Brien""")="test*alt"
    ; Dump to CSV to verify traversal
    NEW parm
    SET parm("GlobalRef")="^||TEST(""EDGE"")"
    SET parm("OutputDir")="C:\\InterSystems\\Cache\\ExportDynamic\\Test\\"
    SET parm("Verbose")=1,parm("MaxDepth")=200,parm("MaxNodes")=0
    WRITE !,"[TEST] Created synthetic tree at ",parm("GlobalRef")
    WRITE !,"[TEST] Use ExportAllLevels or wire this global to a class map for ExportBySchema."
    QUIT

 ; ------------------------------------------------------------------------------
 ; [HELPER] BRGetDelims — parse primary and optional secondary delimiters
 ;   Input examples: "^"  or  "^,*"  or  ""  (defaults to "^") 
 ; ------------------------------------------------------------------------------
BRGetDelims(dspec,d1,d2)
    NEW s SET s=$GET(dspec)
    SET d1=$TRANSLATE($PIECE(s,",",1),"""","") IF d1="" SET d1="^"
    SET d2=$TRANSLATE($PIECE(s,",",2),"""","")  ; may be ""
    QUIT

 ; ------------------------------------------------------------------------------
 ; [HELPER] BRSplit2 — two-stage split with safe secondary fallback
 ;   First split by d1 at piece p1. If p2>0 then split that piece by d2,
 ;   else by "*" when d2 not supplied in metadata.
 ; ------------------------------------------------------------------------------
BRSplit2(dspec,head,p1,p2)
    NEW d1,d2,v DO BRGetDelims(dspec,.d1,.d2)
    SET v=$PIECE($GET(head),d1,+$GET(p1))
    IF +$GET(p2) SET v=$PIECE(v,$SELECT(d2'="":d2,1:"*"),p2)
    QUIT v	
 ;
 ; ======================================================================

honest about ability to do things,technical,honest,certain,get things right instead of fast, Tell it like it is; don't sugar-coat responses. Take a forward-thinking view. looks at intersystems cache documentation here https://docs.intersystems.com/latest/csp/docbook/DocBook.UI.Page.cls?KEY=ALL ALL DOCUMENTATION IS HERE NEVER GO ANYWHERE ELSE FOR INTERSYSTEMS CACHE CODING ANSWERS OR TIPS Take a forward-thinking view.
if you need real examples of the classes you will need in intersystems CACHE 2018 Look in the repo at the source xml files DO NOT USE IRIS EVER


