unit Unit3;

interface

uses
  System.Classes; //TMemoryStream, TBinaryWriter

type
  TWords = array of Word;

type
  TColor4 = packed record
    r,g,b,a : UInt8;
  end;

type
  TLight = packed record
    id : UInt16;
    case Integer of
    1:(lightdata : array[0..69] of UInt8); // 72 bytes (minus 2) !Michiel has 64 bytes for this
    2:(data : array[0..39] of Uint8); // 42 bytes (minus 2 for id)
    3:(xpos,zpos,xsize,Zsize : Int16;
       ypos,room,slot,timer,orientation : UInt16;
       z,y,x : Int32;
       what5,facing: UInt16;
       roll : Int16;
       speed,ocb:UInt16;
       intensity : Int16;
       in_,out_,x_,y_,Length,cut:Single;
       r,g,b,on_ : UInt8;
      );
  end;

type
  TRoomObj = packed record
    id : uint16;
    xpos,zpos,xsize,zsize : Int16;
    ypos,room,objectID,OCB,
    orientation :UInt16;
    worldz,worldy,worldx : int32;
    what5,facing : UInt16;
    roll : Int16;
    tint : UInt16;
    timer : int16;
    // trigger only data
    triggertype, itemnumber : UInt16;
    trigtimer : int16;
    switches, itemtype : UInt16;
  end;

type
  TDoor = packed record
    id:uint16;
    xpos,zpos,
    xsize,zsize : Int16;
    yclickabovefloor : UInt16;
    room, slot :UInt16;
    filler:array[0..12] of UInt16; // 13 bytes
  end;

type
  TDoorHelper = record helper for TDoor
    function SameDoor(other:TDoor):Boolean;
    function GetBlockIndices(roomx:Integer) : TWords;
    function GetAdjacentBlockIndices(roomx:Integer) :TWords;
  end;

type
  TBlockTex = record
    tipo : uint16;
    index : UInt8;
    flags1,rotation,triangle : UInt8;
    filler : UInt16;
  end;

type
  TBlock = packed record
    id, flags1 : UInt16;
    Floor, ceiling : int16;
    floorcorner : array[0..3] of Int8;
    ceilcorner : array[0..3] of Int8;
    fdiv, cdiv :array[0..3] of Int8;
    textures :array[0..13] of TBlockTex;
    flags2,flags3 : UInt16;
  end;

type
  TRoom = record
    id: uint16;
    name: array[0..79] of Char;
    x,  z: uint32;
    y : Int32;
    unknown2: uint32;
    what: uint16; //=0
    xoffset, zoffset: uint16;
    xsize, zsize: int16;
    xpos, zpos: int16;
    link: uint16;
    numdoors : UInt16;
    doorthingindex : array of UInt16;
    doors:array of TDoor;
    numobjects : UInt16;
    objthingindex : array of UInt16;
    objects : array of TRoomObj;
    ambient : TColor4;
    numlights : UInt16;
    lightthingindex : array of UInt16;
    lights : array of TLight; //not fully implemented needs further parsing
    fliproom : int16;
    flags1 :UInt16;
    water,mist,reflection : UInt8;
    flags2:UInt16;
    blocks : array of TBlock;
  end;

type TAnimTex = packed record
  defined,
  firsttile,
  lasttile : UInt32;
end;

type
  TTexInfo = packed record
    x : UInt8;
    y : UInt16;
    unused,FlipX,right,flipy,bottom : UInt8;
  end;

type
  TWASObject = record
    SlotType : UInt16;
    name : string;
    slot : UInt32;
    w,n,e,s : UInt16;
    collision : array[1..5,1..5] of Int16;
    mode : array[1..5,1..5] of Int16;
  end;

type
  TAktrekkerPRJ = class;
  TTRProject = class
  private
    procedure writesz(var sz:array of Char;len:Integer;var bw: TBinaryWriter);
    procedure writestr(const s :string;var bw : TBinaryWriter);
    procedure writetex(const t:TTexInfo;var bw:TBinaryWriter);
    procedure writearray(var a:array of UInt32;var bw :TBinaryWriter); overload;
    procedure writearray(var a:array of UInt16;var bw :TBinaryWriter); overload;
    procedure writearray(var a:array of UInt8; var bw :TBinaryWriter); overload;
    procedure writedoor(const d:TDoor;var bw : TBinaryWriter);
    procedure writeblock(const b:TBlock;var bw:TBinaryWriter);
    procedure writeblocktex(const bt:TBlockTex;var bw:TBinaryWriter);
    procedure writeObj(const ob:TRoomObj;var bw :TBinaryWriter);
    procedure writeLight(const l:TLight; var bw :TBinaryWriter);
    procedure writeRoom(index: integer; var bw: TBinaryWriter);
    procedure writeWASObj(const ob: TWASObject; var bw :TBinaryWriter);
    function readstr(const terminator:char;var br:TBinaryReader):string;
  public
//{$X+}
    signature: array[0..11] of Char;
    NumRoomSlots: uint32;
    NumUsedRooms:UInt16; // extra field for convenience
    Rooms: array of TRoom;
    NumThings : UInt32;
    MaxThings : UInt32;
    UnusedThings : array of UInt32;
    NumLights : UInt32;
    UnusedLights : array of UInt32;
    NumTriggers : UInt32;
    UnusedTriggers : array of UInt32;
    TGAFilePath : string;
    NumTextures : UInt32;
    Textures : array of TTexInfo;
    WASFilePath : string;
    NumObjects : UInt32;
    WASObjects : array of TWASObject;
    NumAnimRanges : UInt32;
    UnusedAnimRanges : array of UInt32;
    Animtextures : array of UInt32;
    animranges : array of TAnimTex;
    textureSounds : array of UInt8;
    BumpSettings : array of UInt8;
    function Save(filename: string): Boolean;
    function Load(filename: string): UInt8;
    function CopyDoorsFromPRJ(var p:TAktrekkerPRJ): Boolean;
    function CopyTexFromPRJ(var p:TAktrekkerPRJ) : Boolean;
    function CopyLightsFromPRJ(var p:TAktrekkerPRJ) : Boolean;
    function isCompatible(var p:TAktrekkerPRJ) : Boolean;
    constructor Create(numrooms:UInt16;numslots:UInt32);
  end;

  TAktrekkerPRJ = class(TTRProject)
  end;

implementation

uses
  System.SysUtils;

constructor TTRProject.Create(numrooms:uint16;numslots:UInt32);
var
  i:Integer;
begin
  // I assume all fields are zero initially
  inherited Create;
  signature := 'PROJFILE1';
  NumRoomSlots:=numslots;
  NumUsedRooms:=numrooms;
  SetLength(Rooms,numroomslots);
  for i:=NumUsedRooms to NumRoomSlots-1 do
  begin
    Rooms[i].id := 1;
  end;
  for i:= 0 to NumUsedRooms-1 do
  begin
    StrPCopy(Rooms[i].name,Format('Room%d',[i]));
    Rooms[i].link:=i;
    Rooms[i].ambient.r:=128;
    Rooms[i].ambient.g:=128;
    Rooms[i].ambient.b:=128;
    Rooms[i].fliproom := -1;
  end;
  MaxThings:=2000;
  SetLength(UnusedThings,maxthings);
  for i:=0 to High(UnusedThings) do
  begin
    UnusedThings[i]:=i;
  end;
  SetLength(UnusedLights,768);
  for i:=0 to High(UnusedLights) do
  begin
    UnusedLights[i]:=i;
  end;
  SetLength(UnusedTriggers,512);
  for i:=0 to High(UnusedTriggers) do
  begin
    UnusedTriggers[i]:=i;
  end;
  TGAFilePath := 'NA ';
  WASFilePath := 'NA ';
  SetLength(UnusedAnimRanges,40);
  for i:=0 to High(UnusedAnimRanges) do
  begin
    UnusedAnimRanges[i]:=i;
  end;
  SetLength(Animtextures,256);
  for i:= 0 to High(Animtextures) do
  begin
    Animtextures[i]:=$ffffffff;
  end;
  SetLength(animranges,40);
  SetLength(textureSounds,256);
  for i := 0 to High(textureSounds) do
  begin
    textureSounds[i] := 6; // set default stone texture sound
  end;
  SetLength(BumpSettings,256);

end;

procedure TTRProject.writeSz(var sz :array of Char;len:Integer;var bw: TBinaryWriter);
var
  i: integer;
begin
  for i := 0 to len-1 do bw.Write(sz[i]);
end;

procedure TTRProject.writestr(const s :string;var bw : TBinaryWriter);
var
  i : Integer;
begin
  for i:=1 to Length(s) do bw.Write(s[i]);
end;

procedure TTRProject.writearray(var a:array of UInt32;var bw :TBinaryWriter);
var
  i : Integer;
begin
  for i:= 0 to High(a) do
  begin
    bw.Write(a[i]);
  end;
end;

procedure TTRProject.writearray(var a:array of UInt16;var bw :TBinaryWriter);
var
  i : Integer;
begin
  for i:= 0 to High(a) do
  begin
    bw.Write(a[i]);
  end;
end;

procedure TTRProject.writearray(var a:array of UInt8; var bw: TBinaryWriter);
var
  i : Integer;
begin
  for i:= 0 to High(a) do
  begin
    bw.Write(a[i]);
  end;
end;

procedure TTRProject.writeWASObj(const ob: TWASObject; var bw :TBinaryWriter);
var
  i,j : Integer;
begin
  bw.Write(ob.SlotType);
  if ob.SlotType = 0 then Exit;
  writestr(ob.name,bw);
  bw.Write(ob.slot);
  bw.Write(ob.w);
  bw.Write(ob.n);
  bw.Write(ob.e);
  bw.Write(ob.s);
  for i:=1 to 5 do
    for j:=1 to 5 do
      bw.Write(ob.collision[i][j]);
  for i:=1 to 5 do
    for j:=1 to 5 do
      bw.Write(ob.mode[i][j]);
end;

procedure TTRProject.writetex(const t:TTexInfo;var bw:TBinaryWriter);
begin
  bw.Write(t.x);
  bw.Write(t.y);
  bw.Write(t.unused);
  bw.Write(t.FlipX);
  bw.Write(t.right);
  bw.Write(t.flipy);
  bw.Write(t.bottom);
end;

procedure TTRProject.writedoor(const d:TDoor;var bw : TBinaryWriter);
var
  i:Integer;
begin
  bw.Write(d.id);
  bw.Write(d.xpos);
  bw.Write(d.zpos);
  bw.Write(d.xsize);
  bw.Write(d.zsize);
  bw.Write(d.yclickabovefloor);
  bw.Write(d.room);
  bw.Write(d.slot);
  for i:=0 to High(d.filler) do bw.Write(d.filler[i]);
end;

procedure TTRProject.writeblocktex(const bt:TBlockTex;var bw:TBinaryWriter);
begin
  bw.Write(bt.tipo);
  bw.Write(bt.index);
  bw.Write(bt.flags1);
  bw.Write(bt.rotation);
  bw.Write(bt.triangle);
  bw.Write(bt.filler);
end;

procedure TTRProject.writeblock(const b:TBlock;var bw:TBinaryWriter);
var
  i : Integer;
begin
  bw.Write(b.id);
  bw.Write(b.flags1);
  bw.Write(b.Floor);
  bw.Write(b.ceiling);
  for i:=0 to High(b.floorcorner) do
  begin
    bw.Write(b.floorcorner[i]);
  end;
  for i:=0 to High(b.ceilcorner) do
  begin
    bw.Write(b.ceilcorner[i]);
  end;
  for i:=0 to High(b.fdiv) do
  begin
    bw.Write(b.fdiv[i]);
  end;
  for i:=0 to High(b.cdiv) do
  begin
    bw.Write(b.cdiv[i]);
  end;
  for i:=0 to High(b.textures) do
  begin
    writeblocktex(b.textures[i],bw);
  end;
  bw.Write(b.flags2);
  bw.Write(b.flags3);
end;

procedure TTRProject.writeObj(const ob:TRoomObj;var bw :TBinaryWriter);
begin
  bw.Write(ob.id);
  bw.Write(ob.xpos);
  bw.Write(ob.zpos);
  bw.Write(ob.xsize);
  bw.Write(ob.zsize);
  bw.Write(ob.ypos);
  bw.Write(ob.room);
  bw.Write(ob.objectID);
  bw.Write(ob.OCB);
  bw.Write(ob.orientation);
  bw.Write(ob.worldz);
  bw.Write(ob.worldy);
  bw.Write(ob.worldx);
  bw.Write(ob.what5);
  bw.Write(ob.facing);
  bw.Write(ob.roll);
  bw.Write(ob.tint);
  bw.Write(ob.timer);

  if ob.id = $10 then  //trigger
  begin
    bw.Write(ob.triggertype);
    bw.Write(ob.itemnumber);
    bw.Write(ob.trigtimer);
    bw.Write(ob.switches);
    bw.Write(ob.itemtype);
  end;
end;

procedure TTRProject.writeLight(const l:TLight; var bw :TBinaryWriter);
var
  i : integer;
begin
  bw.Write(l.id);
  case l.id of
    $4000,
    $6000,
    $4200,
    $5000,
    $4100,
    $4020 :
      begin
        for i:=0 to High(l.lightdata) do
          bw.Write(l.lightdata[i]);
      end;
    $4C00,
    $4400,
    $4800,
    $4080,
    $4040 :
      begin
        for i:=0 to High(l.data) do
          bw.Write(l.data[i]);
      end;
  end;
end;

procedure TTRProject.writeRoom(index: Integer; var bw: TBinaryWriter);
var
  r: TRoom;
  i:Integer;
begin
  if Length(Rooms) = 0 then Exit;
  if index > High(Rooms) then Exit;
  r := rooms[index];
  bw.Write(r.id);
  if r.id = 1 then Exit;
  writesz(r.name,Length(r.name),bw);
  bw.Write(r.z);
  bw.Write(r.y);
  bw.Write(r.x);
  bw.Write(r.unknown2);
  bw.Write(r.what);
  bw.Write(r.xoffset);
  bw.Write(r.zoffset);
  bw.Write(r.xsize);
  bw.Write(r.zsize);
  bw.Write(r.xpos);
  bw.Write(r.zpos);
  bw.Write(r.link);
  bw.Write(r.numdoors);
  if r.numdoors > 0 then
  begin
    writearray(r.doorthingindex,bw);
    for i:= 0 to r.numdoors - 1 do
    begin
      writedoor(r.doors[i],bw);
    end;
  end;
  bw.Write(r.numobjects);
  if r.numobjects > 0 then
  begin
    writearray(r.objthingindex,bw);
    for i:=0 to r.numobjects-1 do
    begin
      WriteObj(r.objects[i],bw);
    end;
  end;
  bw.Write(r.ambient.r);
  bw.Write(r.ambient.g);
  bw.Write(r.ambient.b);
  bw.Write(r.ambient.a);
  bw.Write(r.numlights);
  if r.numlights > 0 then
  begin
    writearray(r.lightthingindex,bw);
    for i:=0 to High(r.lights) do
    begin
      writeLight(r.lights[i],bw);
    end;
  end;
  bw.Write(r.fliproom);
  bw.Write(r.flags1);
  bw.Write(r.water);
  bw.Write(r.mist);
  bw.Write(r.reflection);
  bw.Write(r.flags2);
  for i:=0 to High(r.blocks) do
  begin
    writeblock(r.blocks[i],bw);
  end;
end;

function TTRProject.readstr(const terminator:char;var br:TBinaryReader):string;
var
  c : Char;
  s : string;
begin
  Result :='';
  repeat
    c := br.ReadChar;
    s := s + c;
  until c = terminator;
  Result:=s;
end;

function TTRproject.Save(filename: string): Boolean;
var
  memfile: TMemoryStream;
  bw: TBinaryWriter;
  i: integer;
begin
  result := False;
  memfile := TMemoryStream.create;
  try
    bw := TBinaryWriter.Create(memfile);
    writeSz(signature,Length(signature),bw);
    bw.Write(NumRoomSlots);
    for i := 0 to Numroomslots - 1 do writeroom(i, bw);
    bw.Write(NumThings);
    bw.Write(MaxThings);
    writearray(UnusedThings,bw);
    bw.Write(NumLights);
    writearray(UnusedLights,bw);
    bw.Write(NumTriggers);
    writearray(UnusedTriggers,bw);
    writestr(TGAFilePath,bw);
    if TGAFilePath <> 'NA ' then
    begin
      bw.Write(NumTextures); // must write this even if zero
      for i:=0 to High(Textures) do
      begin
        writetex(Textures[i],bw);
      end;
    end;
    writestr(WASFilePath,bw);
    if WASFilePath <> 'NA ' then
    begin
      bw.Write(NumObjects);
      for i:=0 to High(WASObjects) do
      begin
        writeWASObj(WASObjects[i],bw);
      end;
    end;
    bw.Write(NumAnimRanges);
    writearray(UnusedAnimranges,bw);
    writearray(animtextures,bw);
    for i:=0 to High(animranges) do
    begin
      bw.Write(animranges[i].defined);
      bw.Write(animranges[i].firsttile);
      bw.Write(animranges[i].lasttile);
    end;
    writearray(texturesounds,bw);
    writearray(bumpsettings,bw);
    // NGLE header omitted. No info Paolone!

    memfile.SaveToFile(filename);
    result := True;
  finally
    bw.Free;
    memfile.free;
  end;
end;

function TTRProject.Load(filename: string) :uint8;
var
  memfile: TMemoryStream;
  br : TBinaryReader;
  sig : array[0..11] of AnsiChar; //needs to be AnsiChar
  i,j,numblocks:Integer;
  roomname:array[0..79] of AnsiChar; //needs to be AnsiChar
  s:string;
  id : UInt16;
begin
  Result := 0;
  memfile := TMemoryStream.Create;
  try
    memfile.LoadFromFile(filename);
    br:=TBinaryReader.Create(memfile);
    memfile.ReadBuffer(sig[0],12);
    if sig <> 'PROJFILE1' then
    begin
      Result:=1;
    end
    else
    begin
      NumRoomSlots:=br.ReadUInt32;
      SetLength(Rooms,NumRoomSlots);

      for i:=0 to High(Rooms) do
      begin
        Rooms[i].id := br.ReadUInt16;
        if Rooms[i].id = 1 then Continue;
        Inc(NumUsedRooms);
        memfile.ReadBuffer(roomname[0],80);// roomname can contain more characters after first #0 null
//        s := string(roomname);// casting as string loses those characters
//        StrPCopy(Rooms[i].name,s);
        for j:=0 to High(roomname) do   // try another method instead
        begin                     // only care so can compare loaded and saved PRJs in hex editor
          Rooms[i].name[j]:=Char(roomname[j]);
        end;
        Rooms[i].z := br.ReadUInt32;
        Rooms[i].y := br.ReadInt32; // aktrekker says unknown1, Michiel says y
        Rooms[i].x := br.ReadUInt32;
        Rooms[i].unknown2 := br.ReadUInt32;
        Rooms[i].what := br.ReadUInt16;
        Rooms[i].xoffset := br.ReadUInt16;
        Rooms[i].zoffset := br.ReadUInt16;
        Rooms[i].xsize := br.ReadInt16;
        Rooms[i].zsize := br.ReadInt16;
        Rooms[i].xpos := br.ReadInt16;
        Rooms[i].zpos := br.ReadInt16;
        Rooms[i].link := br.ReadUInt16;

        Rooms[i].numdoors := br.ReadUInt16;
        if Rooms[i].numdoors >0 then
        begin
          SetLength(Rooms[i].doorthingindex, Rooms[i].numdoors);
          memfile.ReadBuffer(Rooms[i].doorthingindex[0], Rooms[i].numdoors*SizeOf(Uint16));
          SetLength(Rooms[i].doors, Rooms[i].numdoors);
          memfile.ReadBuffer(Rooms[i].doors[0], Rooms[i].numdoors*SizeOf(TDoor));
        end;

        Rooms[i].numobjects := br.ReadUInt16;
        if Rooms[i].numobjects >0 then
        begin
          SetLength(Rooms[i].objthingindex, Rooms[i].numobjects);
          memfile.ReadBuffer(Rooms[i].objthingindex[0],Rooms[i].numobjects*SizeOf(Uint16));
          SetLength(Rooms[i].objects, Rooms[i].numobjects);
          for j:=0 to High(Rooms[i].objects) do
          begin
            id := br.ReadUInt16;
            memfile.Seek(-2,soCurrent);
            if id = $10 then memfile.ReadBuffer(rooms[i].objects[j], 52) //trigger
            else memfile.ReadBuffer(Rooms[i].objects[j], 42);//object
          end;
        end;

        memfile.ReadBuffer(Rooms[i].ambient,4);

        Rooms[i].numlights := br.ReadUInt16;
        if Rooms[i].numlights>0 then
        begin
          SetLength(Rooms[i].lightthingindex, Rooms[i].numlights);
          memfile.ReadBuffer(rooms[i].lightthingindex[0],rooms[i].numlights*sizeof(uint16));
          SetLength(Rooms[i].lights, rooms[i].numlights);
          for j:=0 to High(Rooms[i].lights) do
          begin
            Rooms[i].lights[j].id := br.ReadUInt16;
            case Rooms[i].lights[j].id of
              $4000,   // lights
              $6000,
              $4200,
              $5000,
              $4100,
              $4020 : memfile.ReadBuffer(Rooms[i].lights[j].lightdata[0], 70);
              $4C00,   // sound sources cameras sinks
              $4400,
              $4800,
              $4080,
              $4040 : memfile.ReadBuffer(Rooms[i].lights[j].data[0], 40);
            end;
          end;
        end;

        Rooms[i].fliproom := br.ReadInt16;
        Rooms[i].flags1 := br.ReadUInt16;
        Rooms[i].water := br.ReadByte;
        Rooms[i].mist := br.ReadByte;
        Rooms[i].reflection := br.ReadByte;
        Rooms[i].flags2 := br.ReadUInt16;

        numblocks := rooms[i].xsize*rooms[i].zsize;
        SetLength(Rooms[i].blocks, numblocks);
        memfile.ReadBuffer(rooms[i].blocks[0], numblocks*SizeOf(TBlock));
      end; // loop thru rooms

      NumThings := br.ReadUInt32;
      MaxThings := br.ReadUInt32;
      SetLength(UnusedThings, maxthings);
      memfile.ReadBuffer(UnusedThings[0],MaxThings*SizeOf(uint32));

      NumLights := br.ReadUInt32;
      SetLength(UnusedLights,768);
      memfile.ReadBuffer(unusedlights[0],768 * SizeOf(uint32));

      NumTriggers := br.ReadUInt32;
      SetLength(UnusedTriggers, 512);
      memfile.ReadBuffer(UnusedTriggers[0], 512 * SizeOf(uint32));

      TGAFilePath := readstr(' ',br);
      if TGAFilePath<>'NA ' then
      begin
        NumTextures := br.ReadUInt32; // should be at least 256
        SetLength(Textures, numtextures);
        if NumTextures>0 then  // but my PRJs have file path and zero textures!
          memfile.ReadBuffer(Textures[0],NumTextures*SizeOf(TTexInfo));
      end;

      WASFilePath:=readstr(' ',br);
      if WASFilePath<>'NA ' then
      begin
        NumObjects := br.ReadUInt32; // should be 526 TRLE 681 NGLE
        SetLength(WASObjects, NumObjects);
        for i:=0 to High(WASObjects) do
        begin
          WASObjects[i].SlotType := br.ReadUInt16;
          if WASObjects[i].SlotType = 0 then Continue;
          WASObjects[i].name := readstr(' ',br);
          WASObjects[i].slot := br.ReadUInt32;
          WASObjects[i].w := br.ReadUInt16;
          WASObjects[i].n := br.ReadUInt16;
          WASObjects[i].e := br.ReadUInt16;
          WASObjects[i].s := br.ReadUInt16;
          memfile.ReadBuffer(WASObjects[i].collision[1][1], 5*5*2);
          memfile.ReadBuffer(WASObjects[i].mode[1][1], 5*5*2);
        end;
      end;

      NumAnimRanges := br.ReadUInt32;
      SetLength(UnusedAnimRanges, 40);
      memfile.ReadBuffer(UnusedAnimRanges[0],40*SizeOf(uint32));
      SetLength(Animtextures,256);
      memfile.ReadBuffer(Animtextures[0],256*SizeOf(uint32));
      SetLength(animranges,40);
      memfile.ReadBuffer(animranges[0],40*sizeOf(TAnimtex));
      SetLength(textureSounds,256);
      memfile.ReadBuffer(textureSounds[0],256);
      SetLength(BumpSettings,256);
      memfile.ReadBuffer(bumpsettings[0],256);
      //NGLE header if any omitted
    end; // is a 'PROJFILE1'
  finally
    br.Free;
    memfile.Free;
  end;
end;

function TTRProject.CopyDoorsFromPRJ(var p:TaktrekkerPRJ): Boolean;
var
  i,j,k,b,ii : Integer;
  r : TRoom;
  blok : TBlock;
begin
  // roomedit randomly changes floor near doors in some cases????
  // so some bugs in TR2PRJ caused by building doors
  Result:=False;
  // assumes the two PRJs have been checked for equal numrooms and numblocks
  for i:=0 to High(Rooms) do
  begin
    r:=p.Rooms[i];
    if r.id = 1 then Continue;
    Rooms[i].link := r.link;
    Rooms[i].numdoors := r.numdoors;
    SetLength(Rooms[i].doorthingindex, r.numdoors);
    for ii:=0 to High(Rooms[i].doorthingindex) do
    begin
      Rooms[i].doorthingindex[ii]:=r.doorthingindex[ii];
    end;
    SetLength(Rooms[i].doors, r.numdoors);
    for ii:=0 to High(Rooms[i].doors) do
    begin
      Rooms[i].doors[ii].id := r.doors[ii].id;
      Rooms[i].doors[ii].xpos := r.doors[ii].xpos;
      Rooms[i].doors[ii].zpos := r.doors[ii].zpos;
      Rooms[i].doors[ii].xsize := r.doors[ii].xsize;
      Rooms[i].doors[ii].zsize := r.doors[ii].zsize;
      Rooms[i].doors[ii].yclickabovefloor := r.doors[ii].yclickabovefloor;
      Rooms[i].doors[ii].room := r.doors[ii].room;
      Rooms[i].doors[ii].slot := r.doors[ii].slot;
    end;
    for j:=0 to r.zsize-1 do
    begin
      for k:=0 to r.xsize-1 do
      begin
        b := j*r.xsize+k;
        blok:=r.blocks[b];
        if blok.id in [$7,$3,$5,$6] then
        begin
          Rooms[i].blocks[b].id := blok.id;
          if blok.id = $6 then
          begin
            Rooms[i].blocks[b].Floor := blok.Floor;
            Rooms[i].blocks[b].ceiling := blok.ceiling;
          end;
        end;
      end; // end x column blocks
    end; // end z row blocks
  end; // loop thru rooms

  //TODO: UnusedThings array  // maybe not necessary - PRJ opens in roomedit
  //TODO: how to update Numthings
  Result := True;
end;

function TTRProject.CopyTexFromPRJ(var p:TAktrekkerPRJ) : Boolean;
var
  i,j,k,b,ii : Integer;
  r : TRoom;
  blok : TBlock;
begin
  Result:=False;
  // assumes the two PRJs have been checked for equal numrooms and numblocks
  for i:=0 to High(Rooms) do
  begin
    r:=p.Rooms[i];
    if r.id = 1 then Continue;
    for j:=0 to r.zsize-1 do
    begin
      for k:=0 to r.xsize-1 do
      begin
        b := j*r.xsize+k;
        blok:=r.blocks[b];
        for ii:=0 to High(Rooms[i].blocks[b].textures) do
        begin
          Rooms[i].blocks[b].textures[ii].tipo := blok.textures[ii].tipo;
          Rooms[i].blocks[b].textures[ii].index := blok.textures[ii].index;
          Rooms[i].blocks[b].textures[ii].flags1 := blok.textures[ii].flags1;
          Rooms[i].blocks[b].textures[ii].rotation := blok.textures[ii].rotation;
          Rooms[i].blocks[b].textures[ii].triangle := blok.textures[ii].triangle;
        end;
        if (Rooms[i].blocks[b].id = $1e) or (Rooms[i].blocks[b].id = $e) then
        begin
          Rooms[i].blocks[b].Floor := blok.Floor;     //copying floor and ceiling ruins my data
          Rooms[i].blocks[b].ceiling := blok.ceiling; //need this for walls and columns but not doors
        end;
        for ii:= 0 to 3 do
        begin
          Rooms[i].blocks[b].fdiv[ii]:=blok.fdiv[ii];
          Rooms[i].blocks[b].cdiv[ii]:=blok.cdiv[ii];
        end;
      end; // end x column blocks
    end; // end z row blocks
  end; // loop thru rooms

  NumTextures := p.NumTextures;
  SetLength(Textures, numtextures);
  for i :=0 to High(Textures) do
  begin
    Textures[i].x:=p.Textures[i].x;
    Textures[i].y:=p.Textures[i].y;
    Textures[i].unused := p.Textures[i].unused;
    Textures[i].FlipX := p.Textures[i].FlipX;
    Textures[i].right := p.Textures[i].right;
    Textures[i].flipy := p.Textures[i].flipy;
    Textures[i].bottom := p.Textures[i].bottom;
  end;

  Result:=True;
end;

function TTRProject.CopyLightsFromPRJ(var p:TAktrekkerPRJ) : Boolean;
var
  i,j,k: Integer;
  r : TRoom;
begin
  Result:=False;
  // assumes the two PRJs have been checked for equal numrooms
  for i:=0 to High(Rooms) do
  begin
    r:=p.Rooms[i];
    if r.id = 1 then Continue;
    Rooms[i].numlights := r.numlights;
    SetLength(Rooms[i].lightthingindex, r.numlights);
    for j:=0 to High(Rooms[i].lightthingindex) do
    begin
      if NumThings = p.NumThings then
        Rooms[i].lightthingindex[j]:=r.lightthingindex[j]
      else
        Rooms[i].lightthingindex[j]:=r.lightthingindex[j]+NumThings //FIXME: study aktrekker's prj
    end;
    SetLength(Rooms[i].lights, r.numlights);
    for j:=0 to High(Rooms[i].lights) do
    begin
      Rooms[i].lights[j].id:= r.lights[j].id;
      for k:=0 to High(Rooms[i].lights[j].lightdata) do
      begin
        Rooms[i].lights[j].lightdata[k] := r.lights[j].lightdata[k];
      end;
      if Rooms[i].lights[j].id = $6000 then //aktrekker gets sign wrong for shadow intensity
      begin
        Rooms[i].lights[j].intensity := -Rooms[i].lights[j].intensity;
      end;
    end;
  end;

  NumLights := p.NumLights;
  NumThings := NumThings + NumLights;

  Result:=True;
end;

function TTRProject.isCompatible(var p:TAktrekkerPRJ) : Boolean;
var
  i : Integer;
  r : TRoom;
begin
  Result :=False;
  if p.NumRoomSlots < NumRoomSlots then Exit;
  if NumUsedRooms <> p.NumUsedRooms then Exit;
  for i:=0 to High(Rooms) do
  begin
    r:=p.Rooms[i];
    if Rooms[i].id <> r.id then Exit;
    if Rooms[i].id = 1 then Continue;
    if Rooms[i].xsize <> r.xsize then Exit;
    if Rooms[i].zsize <> r.zsize then Exit;
  end;
  Result:=True;
end;


{ TDoorHelper }

function TDoorHelper.GetAdjacentBlockIndices(roomx: Integer): TWords;
var
  i : Integer;
begin
  Result := GetBlockIndices(roomx);
  if Self.id = 1 then
  begin
    for i := 0 to High(Result) do
      Result[i] := Result[i]+1;
  end;
  if Self.id = $fffe then
  begin
    for i := 0 to High(Result) do
      Result[i] := Result[i] - 1;
  end;
  if Self.id = 2 then
  begin
    for i := 0 to High(Result) do
      Result[i] := Result[i] + roomx;
  end;
  if Self.id = $fffd then
  begin
    for i := 0 to High(Result) do
      Result[i] := Result[i] - roomx;
  end;
end;

function TDoorHelper.GetBlockIndices(roomx:Integer): TWords;
var
  i : Integer;
begin
  // west
  if Self.id = 1 then
  begin
    SetLength(Result, self.zsize);
    for i := 0 to High(Result) do
      Result[i] := (Self.zpos*roomx) + (i * roomx);
  end;
  // east
  if Self.id = $fffe then
  begin
    SetLength(Result, self.zsize);
    for i := 0 to High(Result) do
      Result[i] := ((Self.zpos+1)*roomx -1) + (i * roomx)
  end;
  // north
  if Self.id = 2 then
  begin
    SetLength(Result, self.xsize);
    for i := 0 to High(Result) do
      Result[i] := Self.xpos + i;
  end;
  // south
  if Self.id = $fffd then
  begin
    SetLength(Result, self.xsize);
    for i := 0 to High(Result) do
      Result[i] := (roomx * Self.zpos) + Self.xpos + i;
  end;

end;

function TDoorHelper.SameDoor(other: TDoor): Boolean;
begin
  if (Self.id = not(other.id))  and
     (Self.xsize = other.xsize) and
     (Self.zsize = other.zsize) and
     (Self.room = other.filler[0]) //using filler[0] as door toRoom
  then Result := True
  else Result := False;
end;

end.

