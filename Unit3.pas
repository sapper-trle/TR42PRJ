unit Unit3;

interface

uses
  System.Classes; //TMemoryStream, TBinaryWriter

type
  TColor4 = record
    r,g,b,a : UInt8;
  end;

type
  TLight = record
    id : UInt16;
    // rest omitted
  end;

type
  TObj = record
    id : uint16;
    //rest omitted
  end;

type
  TDoor = record
    id:uint16;
    xpos,zpos,
    xsize,zsize : Int16;
    yclickabovefloor : UInt16;
    room,thingindex :UInt16;
    filler:array[0..12] of UInt16;
  end;

type TAnimTex = record
  defined,
  firsttile,
  lasttile : UInt32;
end;

type
  TTexInfo = record
    x,y : UInt8;
    page : UInt16;
    FlipX,right,flipy,bottom : UInt8;
  end;

type
  TBlockTex = record
    tipo : uint16;
    index : UInt8;
    flags1,rotation,triangle : UInt8;
    filler : UInt16;
  end;

type
  TBlock = record
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
    doorthingindex : array of UInt16; //not used
    doors:array of TDoor; //not used
    numobjects : UInt16;
    objthingindex : array of UInt16; //not used
    objects : array of TObject; //not implemented
    ambient : TColor4;
    numlights : UInt16;
    lightthingindex : array of UInt16; //not used
    lights : array of TLight; //not implemented
    fliproom : int16;
    flags1 :UInt16;
    water,mist,reflection : UInt8;
    flags2:UInt16;
    blocks : array of TBlock;
  end;

type
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
    procedure writeRoom(index: integer; var bw: TBinaryWriter);
  public
//{$X+}
    signature: array[0..11] of char;
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
    // object data omitted
    NumAnimRanges : UInt32;
    UnusedAnimRanges : array of UInt32;
    Animtextures : array of UInt32;
    animranges : array of TAnimTex;
    textureSounds : array of UInt8;
    BumpSettings : array of UInt8;
    function Save(filename: string): Boolean;
    constructor Create(numrooms:UInt16;numslots:UInt32);
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

procedure TTRProject.writetex(const t:TTexInfo;var bw:TBinaryWriter);
begin
  bw.Write(t.x);
  bw.Write(t.y);
  bw.Write(t.page);
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
  bw.Write(d.thingindex);
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
    // TODO: write object array
  end;
  bw.Write(r.ambient.r);
  bw.Write(r.ambient.g);
  bw.Write(r.ambient.b);
  bw.Write(r.ambient.a);
  bw.Write(r.numlights);
  if r.numlights > 0 then
  begin
    writearray(r.lightthingindex,bw);
    // TODO: write light array
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
      bw.Write(NumTextures);
      for i:=0 to High(Textures) do
      begin
        writetex(Textures[i],bw);
      end;
    end;
    writestr(WASFilePath,bw); //always 'NA ';
    // object data omitted

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

end.

