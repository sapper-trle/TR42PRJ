unit Unit2;

interface

uses
  unit3, // TTRProject type
  Vcl.Graphics, // TBitmap
  System.Generics.Collections, // TList<...>
    Vcl.Samples.Gauges; // TGauge

type
  TFloorType = (Floor,door,tilt,roof,trigger,lava,climb,split1,split2,split3,split4,
                nocol1,nocol2,nocol3,nocol4,nocol5,nocol6,nocol7,nocol8,monkey,trigtrig,beetle);

type
  TParsedFloorData = record
    tipo : TFloorType;
    case TFloorType of
      Floor,trigger,lava,monkey,trigtrig,beetle:();
      door:(toRoom:UInt16);
      tilt,roof:(addX,addZ:int8);
      climb:(n,s,e,w:Boolean);
      split1,split2,split3,split4,nocol1,nocol2,nocol3,nocol4,
      nocol5,nocol6,nocol7,nocol8:(triHLO,triHHi:integer;corners:array[0..3] of UInt16);
  end;

type
  TSector = packed record
    FDindex,BoxIndex :UInt16;
    RoomBelow :UInt8;
    Floor :Int8;
    RoomAbove:UInt8;
    Ceiling:int8;
    floorInfo : TList<TParsedFloorData>;
    hasFD : Boolean;
  end;

type
  TBox = packed record
    xmin,xmax,zmin,zmax :UInt8;
    truefloor,overlapindex : Int16;
  end;

type
  TVertex = packed record
    x,y,z : Int16;
end;

type
  TPortal = packed record
    toRoom : UInt16;
    normal : TVertex;
    vertices : array[0..3] of TVertex;
  end;

type
  TPortalArray = array of TPortal;

type
  TPortalHelper = record helper for TPortal
    function Adjoins(other:TPortal; x,z,x1,z1:Int32):Boolean;
    function Equals(other:TPortal):Boolean;
  end;

type
  TRoomColour = packed record
    b,g,r,a : UInt8;
  end;

type
  TRoom = record
    x,z,yBottom,yTop : Int32;
    numZ,numX : UInt16;
    Sectors:array of TSector;
    num_portals : UInt16;
    Portals: TPortalArray; //array of TPortal;
    colour : TRoomColour;
    altroom : Int16;
    flags:UInt16;
    waterscheme,reverb,altgroup:UInt8;
    isFlipRoom : Boolean; // extra fields for convenience
    original_room : Int16;
  end;


type
  TTRLevel = class
  private
    procedure parseFloorData(FDindex:UInt16;out list:TList<TParsedFloorData>);
  public
    bmp : TBitmap;
    file_version: uint32;
    num_room_textiles, num_obj_textiles, num_bump_textiles: uint16;
    num_sounds: uint32;
    num_rooms: uint16;
    num_animations, num_state_changes, num_anim_dispatches, num_anim_commands,
    num_meshtrees, size_keyframes, num_moveables, num_statics: uint32;
    num_floordata : UInt32;
    num_boxes : UInt32;
    floordata : array of UInt16;
    rooms : array of TRoom;
    boxes : array of TBox;
    constructor Create;
    destructor Destroy;override;
    function Load(filename: string;out gauge:TGauge): uint8;
    function ConvertToPRJ(const filename:string;SaveTGA:Boolean=True;fixFDIVS:Boolean=True): TTRProject;
    procedure MakeDoors(var p:TTRProject; tr2prjlinks:Boolean);
  end;

implementation

uses
  System.SysUtils, Vcl.Dialogs, System.UITypes,
  System.Classes, //TMemoryStream, TBinaryReader
  System.ZLib, // ZDecompressStream() - decompress TR4 data
  Imaging, ImagingTypes, ImagingComponents, // Vampyre Imaging Library for .TGA support
  System.Math; // MaxIntValue()

procedure TTRLevel.parseFloorData(FDindex:UInt16;out list:TList<TParsedFloorData>);

  procedure parseFloorType(arg:UInt16;out f,sub,e:uint16);
  begin
    f:=(arg and $001f);
    sub:=(arg and $7f00) shr 8;
    e:=(arg and $8000) shr 15;
  end;

var
  fd:TParsedFloorData;
  data : UInt16;
  f,sub,e :UInt16;
  k:integer;
begin
  if FDindex=0 then
  begin
    fd.tipo:=Floor;
    list.Add(fd);
    Exit;
  end;
  e := 0;
  k :=0;
  while (e=0) and (FDIndex+k < num_floordata) do
  begin
    data:= floordata[FDindex+k];
    Inc(k);
    parseFloorType(data,f,sub,e);
    fd.tipo:=TFloorType(f);
    if fd.tipo = door then
    begin
      fd.toRoom:=floordata[FDindex+k];
      Inc(k);
    end
    else if (fd.tipo = tilt) or (fd.tipo=roof) then
    begin
      fd.addX:= int8((floordata[FDindex+k] and $ff00) shr 8);
      fd.addZ:= int8(floordata[FDindex+k] and $00ff);
      Inc(k);
    end
    else if fd.tipo = trigger then
    begin
      repeat //skip trigger floordata
        data := floordata[FDIndex+k];
        Inc(k);
      until ((data and $8000) = $8000);
    end
    else if fd.tipo = climb then
    begin
      if (sub and $0001) = $0001 then fd.e :=True else fd.e :=False;
      if (sub and $0002) = $0002 then fd.s :=True else fd.s :=False;
      if (sub and $0004) = $0004 then fd.w :=True else fd.w :=False;
      if (sub and $0008) = $0008 then fd.n :=True else fd.n :=False;
    end
    else if fd.tipo in [split1..nocol8] then
    begin
      fd.triHLo := (data and $03e0) shr 5;
      fd.triHHi := (data and $7c00) shr 10;
      data := floordata[FDindex+k];
      Inc(k);
      fd.corners[0]:=data and $000f;
      fd.corners[1]:=(data and $00f0) shr 4;
      fd.corners[2]:=(data and $0f00) shr 8;
      fd.corners[3]:=(data and $f000) shr 12;
    end;
    list.Add(fd);
  end;

end;


constructor TTRLevel.Create;
begin
  inherited;
  file_version := 0;
end;

destructor TTRLevel.Destroy;
var
  i,j : Integer;
begin
  bmp.Free;
  for i:=0 to High(rooms) do
  begin
    for j:=0 to High(rooms[i].Sectors) do
    begin
      if Assigned(rooms[i].Sectors[j].floorInfo) then
        rooms[i].Sectors[j].floorInfo.Free;
    end;
  end;
  inherited;
end;

function TTRLevel.Load(filename: string; out gauge:TGauge): uint8;
const
  spr = 'spr';
  tex = 'tex';
var
  version: uint32;
  resultado: uint8;
  f: file;
  memfile: TMemoryStream;
  br, br2,br3: TBinaryReader;
  size, size2, size3: uint32;
  geometry1: TMemoryStream;
  geometry: TMemoryStream;
  tex32 : TMemoryStream;
  i, j: integer;
  s: string;
  r:TRoom;
  b:TBitmap;
  col :TColor;
  rd,gr,bl,al:UInt8;
  sectorFD:TList<TParsedFloorData>;
  file_size : Int64;
  height : Integer;
begin
  resultado := 0;
  gauge.Progress:=0;
  if FileExists(filename) then
  begin
    FileMode := 0;
    assignfile(f, filename);
    Reset(f, 1);
    BlockRead(f, version, 4);
    CloseFile(f);
    if (version <> $00345254) and (version <> $63345254) then
    begin
      version := 0;
      resultado := 2;
    end;
  end
  else
  begin
    version := 0;
    resultado := 1;
  end;

  if version = $63345254 then
  begin
    resultado := 3;
    version := 0;
  end;
  gauge.Progress:=1;
  if resultado <> 0 then
  begin
    Result := resultado;
    Exit;
  end;
  memfile := TMemoryStream.Create;
  try
    memfile.LoadFromFile(filename);
    file_size := memfile.Size;
    br := TBinaryReader.Create(memfile);
    try
      file_version := br.ReadUInt32;
      num_room_textiles := br.ReadUInt16;
      num_obj_textiles := br.ReadUInt16;
      num_bump_textiles := br.ReadUInt16;
      memfile.seek(4, soCurrent);
      size := br.ReadUInt32;
      geometry1 := TMemoryStream.Create;
      geometry1.CopyFrom(memfile,size);
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      geometry1.Position:=0;
      tex32:=TMemoryStream.Create;
      ZDecompressStream(geometry1,tex32);
      tex32.Position:=0;
      geometry1.Free;
      if Assigned(bmp) then bmp.Free;
      b:=TBitmap.Create;
      b.PixelFormat:=pf24bit;
      b.Width:=256;
      b.Height:=num_room_textiles * 256; //tex32.Size div 256 div 4;
      br3:=TBinaryReader.Create(tex32);
      for i:=0 to b.Height-1 do
      begin
        if i mod (256*2) = 0 then gauge.AddProgress(1);
        for j := 0 to 255 do
        begin
          bl:=br3.ReadByte;
          gr:=br3.ReadByte;
          rd:=br3.ReadByte;
          al:=br3.ReadByte;
          if al=0 then   // make alpha pixels magenta
          begin
            bl:=255;
            rd:=255;
            gr:=0;
          end;
          col := 0;
          col := col or (bl shl 16);
          col := col or (gr shl 8);
          col := col or rd;
          b.Canvas.Pixels[j,i]:= col;
        end;
      end;
      if num_bump_textiles > 0 then
      begin
        height := b.Height;
        b.Height:= b.Height + ((num_bump_textiles div 2) * 256);
        tex32.Seek(num_obj_textiles*256*256*4, soCurrent);
        for i:=height to b.Height-1 do
        begin
          if i mod (256*2) = 0 then gauge.AddProgress(1);
          for j := 0 to 255 do
          begin
            bl:=br3.ReadByte;
            gr:=br3.ReadByte;
            rd:=br3.ReadByte;
            al:=br3.ReadByte;
            if al=0 then   // make alpha pixels magenta
            begin
              bl:=255;
              rd:=255;
              gr:=0;
            end;
            col := 0;
            col := col or (bl shl 16);
            col := col or (gr shl 8);
            col := col or rd;
            b.Canvas.Pixels[j,i]:= col;
          end;
        end;
      end;
      br3.Free;
      tex32.Free;
      bmp:=b;
      memfile.Seek(4, soCurrent);
      size := br.ReadUInt32;
      memfile.Seek(size, soCurrent); //skip the textiles16
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      memfile.Seek(4, soCurrent);
      size := br.ReadUInt32;
      memfile.Seek(size, soCurrent); //skip the font and sky
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      size2 := br.ReadUInt32;
      size := br.ReadUInt32;
      geometry1 := TMemoryStream.Create;
      geometry1.CopyFrom(memfile, size);
      geometry1.Position := 0;
      geometry := TMemoryStream.Create;
      ZDecompressStream(geometry1, geometry);
      geometry.Position := 0;
      geometry1.Free;
      num_sounds := br.ReadUInt32;
      br2 := TBinaryReader.Create(geometry);
      geometry.Seek(4, soCurrent);
      num_rooms := br2.ReadUInt16;

      SetLength(rooms,num_rooms);
      for i := 0 to High(rooms) do
      begin
        gauge.AddProgress(1);
        r.z := br2.ReadInt32;
        r.x := br2.ReadInt32;
        r.yBottom := br2.ReadInt32;
        r.yTop := br2.ReadInt32;
        size := br2.ReadUInt32; //num room mesh data words
        geometry.Seek(size * 2, soCurrent); //skip verts, quads, tris, sprites
        r.num_portals := br2.ReadUInt16; //num portals
        SetLength(r.Portals,r.num_portals);
        for j := 1 to r.num_portals do
        begin
          geometry.ReadBuffer(r.Portals[j-1],32);
        end;
        r.numX := br2.ReadUInt16;   // X is east west in RoomEdit - opposite of what TRosetta stone says!
        r.numZ := br2.ReadUInt16;   // Z is north south as per RoomEdit grid
        SetLength(r.Sectors, r.numX*r.numZ);
        for j := 1 to r.numX * r.numZ do
        begin
          geometry.ReadBuffer(r.Sectors[j-1],8); // X columns, Z rows
          r.Sectors[j-1].hasFD:=False;
        end;
        geometry.ReadBuffer(r.colour,4);
        size := br2.ReadUInt16; //num lights
        for j := 1 to size do
        begin
          geometry.Seek(46, soCurrent); //skip lights
        end;
        size := br2.ReadUInt16; //num static meshes
        for j := 1 to size do
        begin
          geometry.Seek(20, soCurrent); //skip static meshes
        end;
        r.altroom := br2.ReadInt16;
        r.flags := br2.ReadUInt16;
        r.waterscheme := br2.ReadByte;
        r.reverb := br2.ReadByte;
        r.altgroup := br2.ReadByte;
        r.isFlipRoom := False;
        r.original_room := -1;
        rooms[i] := r;
      end;
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      num_floordata := br2.ReadUInt32;
      SetLength(floordata,num_floordata);
      geometry.ReadBuffer(floordata[0],num_floordata*2);
      size := br2.ReadUInt32; //num object mesh data words
      geometry.Seek(size * 2, soCurrent); //skip object meshes
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      size := br2.ReadUInt32; //num mesh pointers
      geometry.Seek(size * 4, soCurrent); //skip mesh pointers
      num_animations := br2.ReadUInt32;
      geometry.Seek(num_animations * 40, soCurrent);
      num_state_changes := br2.ReadUInt32;
      geometry.Seek(num_state_changes * 6, soCurrent);
      num_anim_dispatches := br2.ReadUInt32;
      geometry.Seek(num_anim_dispatches * 8, soCurrent);
      num_anim_commands := br2.ReadUInt32;
      geometry.Seek(num_anim_commands * 2, soCurrent);
      num_meshtrees := br2.ReadUInt32;
      geometry.Seek(num_meshtrees * 4, soCurrent);
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      size_keyframes := br2.ReadUInt32;
      geometry.Seek(size_keyframes * 2, soCurrent);
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      num_moveables := br2.ReadUInt32;
      geometry.Seek(num_moveables * 18, soCurrent);
      num_statics := br2.ReadUInt32;
      geometry.Seek(num_statics * 32, soCurrent);
      s := br2.ReadChar;
      s := s + br2.ReadChar;
      s := s + br2.ReadChar;
      s := LowerCase(s);
      if s <> spr then
      begin
        MessageDlg('SPR landmark not read correctly!',mtError,[mbOK],0);
        resultado:=4;
      end;
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      size := br2.ReadUInt32; // num sprite tex
      geometry.Seek(size * 16, soCurrent);  //skip sprite tex
      size := br2.ReadUInt32; // num sprite seq
      geometry.Seek(size * 8, soCurrent);  //skip sprite seq
      size := br2.ReadUInt32; // num cameras
      geometry.Seek(size * 16, soCurrent);  //skip cameras
      size := br2.ReadUInt32; // num flyby cams
      geometry.Seek(size * 40, soCurrent);  //skip flyby cams
      size := br2.ReadUInt32; // num sound sources
      geometry.Seek(size * 16, soCurrent);  //skip sound sources
      num_boxes := br2.ReadUInt32; // num Boxes
      SetLength(boxes,num_boxes);
      geometry.ReadBuffer(boxes[0],num_boxes *8);
      gauge.Progress:= Trunc(memfile.Position / file_size * gauge.MaxValue);
      size := br2.ReadUInt32; // num overlaps
      geometry.Seek(size * 2, soCurrent);  //skip overlaps
      geometry.Seek(num_boxes * 20, soCurrent);  //skip zones
      size := br2.ReadUInt32; // num anim tex words
      geometry.Seek(size * 2, soCurrent);  //skip anim tex words
      geometry.Seek(1,soCurrent); // skip uv ranges
      s := br2.ReadChar;
      s := s + br2.ReadChar;
      s := s + br2.ReadChar;
      s := LowerCase(s);
      if s <> tex then
      begin
        MessageDlg('TEX landmark not read correctly!',mtError,[mbOK],0);
        resultado := 4;
      end;
      size := br2.ReadUInt32; // num textures
      geometry.Seek(size * 38, soCurrent);  //skip textures
      gauge.Progress:= gauge.MaxValue;
      // rest of TR4 not read
      br2.Free;
      geometry.Free;
    finally
      br.Free;
    end;
  finally
    memfile.Free;
  end;
  if resultado = 0 then
  begin
    for i:=0 to High(rooms) do
    begin
      for j:=0 to High(rooms[i].Sectors) do
      begin
        if rooms[i].Sectors[j].FDindex =0 then Continue;
        sectorFD := TList<TParsedFloorData>.Create;
        rooms[i].Sectors[j].hasFD:=True;
        parseFloorData(rooms[i].Sectors[j].FDindex,sectorFD);
        rooms[i].Sectors[j].floorInfo:=sectorFD;
      end;
      if rooms[i].altroom <> -1 then
        if rooms[i].altroom <= High(rooms) then
        begin
          rooms[rooms[i].altroom].isFlipRoom := True;
          rooms[rooms[i].altroom].original_room := i;
        end;
    end;
  end;
  Result := resultado;
end;

function TTRLevel.ConvertToPRJ(const filename :string;SaveTGA:Boolean=True;fixFDIVS:Boolean=True): TTRProject;
var
  p:TTRProject;
  slots:uint32;
  i,j,k,ii,jj,temp:Integer;
  r1:TRoom;
  sector:TSector;
  s:string;
  img:TImageData;
  b:Integer;
  fd:TParsedFloorData;
  maxCorner:Word;
  a:array[0..3]of integer;
  po:TPortal;
  isHorizontalDoor : Boolean;
  bx:Uint32;
begin
  if (num_rooms <= 100) then slots:=100
  else if ((num_rooms>100) and (num_rooms<=200)) then slots:=200
  else slots := 300;
  p := TTRProject.Create(num_rooms,slots);
  if SaveTGA and (bmp.Width>0) and (bmp.Height>0) then
  begin
    s := ChangeFileExt(filename,'.tga');
    InitImage(img);
    img.Format := ifR8G8B8;
    ConvertBitmapToData(bmp,img); // make sure bitmap's pixelformat was set to 24!!
    SaveImageToFile(s,img);
    FreeImage(img);

    p.TGAFilePath := ExtractShortPathName(s);
    if LowerCase(p.TGAFilePath)='.tga' then MessageDlg('PRJ TGA filename error',mtError,[mbOK],0);
    p.TGAFilePath := p.TGAFilePath+' ';  // don't forget space
  end;
  for i:=0 to High(rooms) do
  begin
    r1:=rooms[i];
    if r1.isFlipRoom then StrPCopy(p.Rooms[i].name, Format('Flipped Room%d',[r1.original_room]));
    p.Rooms[i].x := r1.x;
//    p.Rooms[i].y := r1.yBottom;
    p.Rooms[i].z := r1.z;
    p.Rooms[i].xsize := r1.numX;
    p.Rooms[i].zsize := r1.numZ;
    p.Rooms[i].xoffset := (20 - r1.numX) div 2;
    p.Rooms[i].zoffset := (20 - r1.numZ) div 2;
    p.Rooms[i].xpos := r1.x div 1024;
    p.Rooms[i].zpos := r1.z  div 1024;
    p.Rooms[i].ambient.r := r1.colour.r;
    p.Rooms[i].ambient.g := r1.colour.g;
    p.Rooms[i].ambient.b := r1.colour.b;
    p.Rooms[i].ambient.a := r1.colour.a;
    p.Rooms[i].fliproom := r1.altroom;
    p.rooms[i].flags1 := r1.flags; // TODO: check they're the same
    if r1.isFlipRoom then p.Rooms[i].flags1 := p.Rooms[i].flags1 or $2;
    p.Rooms[i].isFlipRoom := r1.isFlipRoom;
    p.Rooms[i].flags2 := p.Rooms[i].flags2 or r1.altgroup;
    p.Rooms[i].yBottom := -r1.yBottom div 256;
    p.Rooms[i].yTop := -r1.yTop div 256;
    SetLength(p.Rooms[i].blocks,r1.numZ*r1.numX);
    for j:=0 to r1.numZ-1 do  // rows
    begin
      for k:=0 to r1.numX-1 do  // columns
      begin
        b := j*r1.numX+k; // index to block array and sector array
        sector := r1.Sectors[b];
        p.Rooms[i].blocks[b].id := $1; // default is floor type
        p.Rooms[i].blocks[b].Floor := -sector.Floor;
        p.Rooms[i].blocks[b].ceiling := -sector.Ceiling;
        if (p.Rooms[i].blocks[b].Floor<>127) and fixFDIVS then
        begin
          for ii:=0 to 3 do
          begin
            temp:=-Abs(p.Rooms[i].blocks[b].Floor-(-r1.yBottom div 256));
            p.Rooms[i].blocks[b].fdiv[ii]:= temp;
          end;
        end;
        if (p.Rooms[i].blocks[b].ceiling<>127) and fixFDIVS then
        begin
          for ii:=0 to 3 do
          begin
            temp:=(-r1.yTop div 256)- p.Rooms[i].blocks[b].ceiling;
            p.Rooms[i].blocks[b].cdiv[ii]:= Abs(temp);
          end;
        end;

        // corner grey blocks can be any heights I guess since never seen
        // aktrekker makes them same as corner blocks of inner blocks
        // floor and ceil heights for grey blocks/walls are division lines
        if ((k=0)and(j=0)) or ((k=r1.numX-1)and(j=0)) or ((k=0)and(j=r1.numZ-1)) or ((k=r1.numX-1)and(j=r1.numZ-1)) then
        begin
          p.Rooms[i].blocks[b].id := $1e;
          p.Rooms[i].blocks[b].Floor := 0;
          p.Rooms[i].blocks[b].ceiling := 20;
        end
        // other grey outer blocks
        else if (j=0) or (j=r1.numZ-1) or (k=0) or (k=r1.numX-1) then
        begin
          p.Rooms[i].blocks[b].id := $1e;
          p.Rooms[i].blocks[b].Floor := -r1.yBottom div 256;
          p.Rooms[i].blocks[b].ceiling := -r1.yTop div 256;
        end
        // green wall blocks except those with door FloorData
        // NGLE sometimes switches wall block adjacent to doors to floor block
        // no idea why, definitely wall block in prj file
        // may need to set heights same as neighbours instead of ybottom and ytop
        else if (sector.Floor = -127) then
        begin
          p.Rooms[i].blocks[b].id := $e;
          p.Rooms[i].blocks[b].Floor := -r1.yBottom div 256;
          p.Rooms[i].blocks[b].ceiling := -r1.yTop div 256;
        end
        // black door blocks - no good without door arrays implemented
        // no collision blocks need to be marked as id 1, changed later in floordata loop
        else if (sector.RoomBelow<>255) and (sector.RoomAbove<>255) then
        begin
          p.Rooms[i].blocks[b].id := $7;
        end
        else if (sector.RoomBelow<>255) then
        begin
          p.Rooms[i].blocks[b].id := $3;
        end
        else if (sector.RoomAbove<>255) then
        begin
          p.Rooms[i].blocks[b].id := $5;
        end;

        bx:=(sector.BoxIndex and $7FF0) shr 4;
        if (bx <> $7FF) and (bx < num_boxes) then
        begin
          if (boxes[bx].overlapindex and $8000)=$8000 then
            p.Rooms[i].blocks[b].flags1 := p.Rooms[i].blocks[b].flags1 or $0020;
        end;

        if sector.hasFD then // need to check since no list of floordata for FDindex=0 sectors
        begin
          for ii :=0 to sector.floorInfo.Count-1 do
          begin
            fd:=sector.floorInfo[ii];
            if fd.tipo in [trigger] then Continue; // not implemented
            if fd.tipo = door then
            begin
              isHorizontalDoor:=True;
              for jj := 0 to High(r1.Portals) do
              begin
                po:=r1.Portals[jj];
                // check if door toRoom is above by checking portals
                // if it's not a vertical door then it is a horizontal one
                if po.normal.y = 0 then // check vertical portals
                begin
                  if po.toRoom = fd.toRoom then
                  begin
                    isHorizontalDoor := False;
                    Break;
                  end;
                end;
              end;
              if isHorizontalDoor then // it's a green wall block not a floor block
              begin
                if p.Rooms[i].blocks[b].id = $1 then p.Rooms[i].blocks[b].id := $e;
                if p.Rooms[i].blocks[b].id = $1e then
                begin
                  p.Rooms[i].blocks[b].Floor := -sector.Floor;
                  p.Rooms[i].blocks[b].ceiling := -sector.Ceiling;
                end;
              end
              else
                if p.Rooms[i].blocks[b].id = $1e then
                begin
                  p.Rooms[i].blocks[b].id := $6;
                  p.Rooms[i].blocks[b].Floor := -sector.Floor; // these values wrong
                  p.Rooms[i].blocks[b].ceiling := -sector.Ceiling; // fixed later when doors created
                end;
              Continue;
            end;
            if fd.tipo=beetle then
            begin
              p.Rooms[i].blocks[b].flags2:= p.Rooms[i].blocks[b].flags2 or $0040;
              Continue;
            end;
            if fd.tipo=trigtrig then
            begin
              p.Rooms[i].blocks[b].flags2:= p.Rooms[i].blocks[b].flags2 or $0020;
              Continue;
            end;
            if fd.tipo=climb then
            begin
              if fd.n then p.Rooms[i].blocks[b].flags1:=p.Rooms[i].blocks[b].flags1 or $0200;
              if fd.s then p.Rooms[i].blocks[b].flags1:=p.Rooms[i].blocks[b].flags1 or $0080;
              if fd.w then p.Rooms[i].blocks[b].flags1:=p.Rooms[i].blocks[b].flags1 or $0100;
              if fd.e then p.Rooms[i].blocks[b].flags1:=p.Rooms[i].blocks[b].flags1 or $0040;
              Continue;
            end;
            if fd.tipo=monkey then
            begin
              p.Rooms[i].blocks[b].flags1:=p.Rooms[i].blocks[b].flags1 or $4000;
              Continue;
            end;
            if fd.tipo=lava then
            begin
              p.Rooms[i].blocks[b].flags1:=p.Rooms[i].blocks[b].flags1 or $0010;
              Continue;
            end;
            if fd.tipo=tilt then
            begin // can have both X and Z tilt
              if fd.addX>=0 then
              begin
                p.Rooms[i].blocks[b].floorcorner[2]:=fd.addX;
                p.Rooms[i].blocks[b].floorcorner[3]:=fd.addX;
              end
              else
              begin
                p.Rooms[i].blocks[b].floorcorner[0]:= -fd.addX;
                p.Rooms[i].blocks[b].floorcorner[1]:= -fd.addX;
              end;
              if fd.addZ>=0 then
              begin
                p.Rooms[i].blocks[b].floorcorner[0]:=p.Rooms[i].blocks[b].floorcorner[0]+fd.addZ;
                p.Rooms[i].blocks[b].floorcorner[3]:=p.Rooms[i].blocks[b].floorcorner[3]+fd.addZ;
              end
              else
              begin
                p.Rooms[i].blocks[b].floorcorner[2]:=p.Rooms[i].blocks[b].floorcorner[2]+Abs(fd.addZ);
                p.Rooms[i].blocks[b].floorcorner[1]:=p.Rooms[i].blocks[b].floorcorner[1]+Abs(fd.addZ);
              end;
              p.Rooms[i].blocks[b].Floor := p.Rooms[i].blocks[b].Floor-(Abs(fd.addX)+abs(fd.addZ));
              if fixFDIVS then
              begin
                p.Rooms[i].blocks[b].fdiv[0]:= -Abs(p.Rooms[i].blocks[b].Floor-(-r1.yBottom div 256));
                p.Rooms[i].blocks[b].fdiv[1]:= -Abs(p.Rooms[i].blocks[b].Floor-(-r1.yBottom div 256));
                p.Rooms[i].blocks[b].fdiv[2]:= -Abs(p.Rooms[i].blocks[b].Floor-(-r1.yBottom div 256));
                p.Rooms[i].blocks[b].fdiv[3]:= -Abs(p.Rooms[i].blocks[b].Floor-(-r1.yBottom div 256));
              end;
              Continue;
            end; // tilt
            if fd.tipo=roof then
            begin // can have both X and Z roof
              if fd.addX>=0 then
              begin
                p.Rooms[i].blocks[b].ceilcorner[0]:=-fd.addX;
                p.Rooms[i].blocks[b].ceilcorner[1]:=-fd.addX;
              end
              else
              begin
                p.Rooms[i].blocks[b].ceilcorner[2]:=fd.addX;
                p.Rooms[i].blocks[b].ceilcorner[3]:=fd.addX;
              end;
              if fd.addZ>=0 then
              begin
                p.Rooms[i].blocks[b].ceilcorner[1]:=p.Rooms[i].blocks[b].ceilcorner[1]-fd.addZ;
                p.Rooms[i].blocks[b].ceilcorner[2]:=p.Rooms[i].blocks[b].ceilcorner[2]-fd.addZ;
              end
              else
              begin
                p.Rooms[i].blocks[b].ceilcorner[0]:=p.Rooms[i].blocks[b].ceilcorner[0]+fd.addZ;
                p.Rooms[i].blocks[b].ceilcorner[3]:=p.Rooms[i].blocks[b].ceilcorner[3]+fd.addZ;
              end;
              p.Rooms[i].blocks[b].Ceiling := p.Rooms[i].blocks[b].ceiling+Abs(fd.addX)+abs(fd.addZ);
              if fixFDIVS then
              begin
                p.Rooms[i].blocks[b].cdiv[0]:= Abs((-r1.yTop div 256)- p.Rooms[i].blocks[b].ceiling);
                p.Rooms[i].blocks[b].cdiv[1]:= Abs((-r1.yTop div 256)- p.Rooms[i].blocks[b].ceiling);
                p.Rooms[i].blocks[b].cdiv[2]:= Abs((-r1.yTop div 256)- p.Rooms[i].blocks[b].ceiling);
                p.Rooms[i].blocks[b].cdiv[3]:= Abs((-r1.yTop div 256)- p.Rooms[i].blocks[b].ceiling);
              end;
              Continue;
            end; // roof
            if fd.tipo in [split1,split2,nocol1..nocol4] then  // floor splits
            begin
              // no good for corner heights > 15!!!  maybe need to check geometry vertices
              p.Rooms[i].blocks[b].floorcorner[0]:=fd.corners[0];
              p.Rooms[i].blocks[b].floorcorner[1]:=fd.corners[1];
              p.Rooms[i].blocks[b].floorcorner[2]:=fd.corners[2];
              p.Rooms[i].blocks[b].floorcorner[3]:=fd.corners[3];
              a[0]:=fd.corners[0];
              a[1]:=fd.corners[1];
              a[2]:=fd.corners[2];
              a[3]:=fd.corners[3];
              maxCorner:=maxIntvalue(a);
              p.Rooms[i].blocks[b].Floor := p.Rooms[i].blocks[b].Floor-maxCorner;
              if fd.tipo in [split2,nocol3,nocol4] then // function 8 splits
              begin
                // if corners indicate opposite diagonal
                if (a[1]>Max(a[0],a[2])) or (a[3]>Max(a[0],a[2])) or (a[1]<Min(a[0],a[2])) or (a[3]<Min(a[0],a[2])) then
                  p.Rooms[i].blocks[b].flags3:=p.Rooms[i].blocks[b].flags3 or $1;
                if fd.tipo = nocol3 then
                  p.Rooms[i].blocks[b].flags2:=p.Rooms[i].blocks[b].flags2 or $4;
                if fd.tipo = nocol4 then
                  p.Rooms[i].blocks[b].flags2:=p.Rooms[i].blocks[b].flags2 or $2;
              end;
              if fd.tipo in [split1,nocol1,nocol2] then // function 7 splits
              begin
                // if corners indicate opposite diagonal
                if (a[0]>Max(a[1],a[3])) or (a[2]>Max(a[1],a[3])) or (a[0]<Min(a[1],a[3])) or (a[2]<Min(a[1],a[3])) then
                  p.Rooms[i].blocks[b].flags3:=p.Rooms[i].blocks[b].flags3 or $1;
                if fd.tipo = nocol1 then
                  p.Rooms[i].blocks[b].flags2:=p.Rooms[i].blocks[b].flags2 or $4;
                if fd.tipo = nocol2 then
                  p.Rooms[i].blocks[b].flags2:=p.Rooms[i].blocks[b].flags2 or $2;
              end;
              if fd.tipo in [nocol1..nocol4] then
              begin
                if p.Rooms[i].blocks[b].id = $3 then p.Rooms[i].blocks[b].id := $1;
                if p.Rooms[i].blocks[b].id = $7 then p.Rooms[i].blocks[b].id := $5;//todo check this
              end;
              Continue;
            end; //floor splits
            if fd.tipo in [split3,split4,nocol5..nocol8] then // ceiling splits
            begin
              // no good for corner heights > 15!!!  maybe need to check geometry vertices
              p.Rooms[i].blocks[b].ceilcorner[0]:=-fd.corners[0];
              p.Rooms[i].blocks[b].ceilcorner[1]:=-fd.corners[1];
              p.Rooms[i].blocks[b].ceilcorner[2]:=-fd.corners[2];
              p.Rooms[i].blocks[b].ceilcorner[3]:=-fd.corners[3];
              a[0]:=fd.corners[0];
              a[1]:=fd.corners[1];
              a[2]:=fd.corners[2];
              a[3]:=fd.corners[3];
              maxCorner:=maxIntvalue(a);
              p.Rooms[i].blocks[b].ceiling := p.Rooms[i].blocks[b].ceiling+maxCorner;
              if fd.tipo in [nocol5, nocol7] then
                p.Rooms[i].blocks[b].flags2:=p.Rooms[i].blocks[b].flags2 or $10;
              if fd.tipo in [nocol6, nocol8] then
                p.Rooms[i].blocks[b].flags2:=p.Rooms[i].blocks[b].flags2 or $8;
              if fd.tipo in [nocol5..nocol8] then
              begin
                if p.Rooms[i].blocks[b].id = $5 then p.Rooms[i].blocks[b].id := $1;
                if p.Rooms[i].blocks[b].id = $7 then p.Rooms[i].blocks[b].id := $3;//todo check this
              end;
              Continue;
            end; // ceiling splits
          end; // loop thru FData
        end; // has FData
      end; // loop thru X block columns
    end; // loop thru Z block rows
  end; // loop thru rooms
  Result := p;

  { todo: REHome demo level produces block.floor = 127 causes display problem.
  Floor in tr4 is -126}
end;

procedure TTRLevel.MakeDoors(var p: TTRProject; tr2prjlinks:Boolean);
var
  i, j, k, room, previous_room, firstroom : Integer;
  minx, maxx, minz, maxz : Integer;
  portal : TPortal;
  portalarray : TPortalArray; //array of TPortal;
  r : TRoom;
  d, dd : TDoor;
  doorcount : Integer;
  bloks, bloks2 : TWords;
  found:Boolean;
  rm : Word;
begin
  // Uses aktrekker's method of using portals to determine doors

  // aktrekker's TR2PRJ crashes making doors if duplicate portals - see aldwych level
  // so check and remove duplicate portals if any
  for i := 0 to High(rooms) do
  begin
    r := rooms[i];
    if r.num_portals = 0 then Continue;
    SetLength(portalarray, 0);
    for j := 0 to (r.num_portals-1)-1 do
    begin
      k := j+1;
      found := False;
      while (k <= (r.num_portals-1)) and (found = False) do
      begin
        if r.Portals[j].Equals(r.Portals[k]) then
          found := True
        else
          inc(k);
      end;
      if not found then
      begin
        SetLength(portalarray, length(portalarray)+1);
        portalarray[High(portalarray)] := r.Portals[j];
      end;
    end; // loop thru portals
    SetLength(portalarray, length(portalarray)+1);
    portalarray[High(portalarray)] := r.Portals[r.num_portals-1];
    if Length(portalarray) < Length(r.Portals) then
    begin
{$IFDEF DEBUG}
      MessageDlg(Format('Room %d - duplicate portal removed',[i]), mtWarning,[mbOK],0);
{$ENDIF}
      rooms[i].num_portals := Length(portalarray);
      rooms[i].Portals := portalarray;
    end;
  end; // loop thru rooms

  // make doors
  doorcount := 0;
  for i := 0 to High(rooms) do
  begin
    r := rooms[i];
    p.Rooms[i].numdoors := r.num_portals;
    SetLength(p.Rooms[i].doors, r.num_portals);
    SetLength(p.Rooms[i].doorthingindex, r.num_portals);

    for j := Low(r.Portals) to High(r.Portals) do
    begin
      portal := r.Portals[j];
      minx := portal.vertices[0].x;
      maxx := minx;
      minz := portal.vertices[0].z;
      maxz := minz;
      for k := 1 to 3 do
      begin
        if portal.vertices[k].x < minx then minx := portal.vertices[k].x;
        if portal.vertices[k].x > maxx then maxx := portal.vertices[k].x;
        if portal.vertices[k].z < minz then minz := portal.vertices[k].z;
        if portal.vertices[k].z > maxz then maxz := portal.vertices[k].z;
      end;
      d.room := i;
      d.yclickabovefloor := 0;
      FillChar(d.filler,SizeOf(d.filler),0);
      d.filler[0] := portal.toRoom; // use filler[0] as toRoom temporarily
      p.Rooms[i].doorthingindex[j] := doorcount;
      // north   //for room mesh though z east/west and x north/south opposite of room edit
      if portal.normal.x = 1 then
      begin
        d.id := 2;
        d.zpos := 0;
        d.zsize := 1;
        d.xpos := minz div 1024;
        d.xsize := (maxz - minz) div 1024;
      end;
      // south
      if portal.normal.x = -1 then
      begin
        d.id := $fffd;
        d.zpos := minx div 1024;
        d.zsize := 1;
        d.xpos := minz div 1024;
        d.xsize := (maxz - minz) div 1024;
      end;
      // west
      if portal.normal.z = 1 then
      begin
        d.id := 1;
        d.xpos := 0;
        d.xsize := 1;
        d.zpos := minx div 1024;
        d.zsize := (maxx - minx) div 1024;
      end;
      // east
      if portal.normal.z = -1 then
      begin
        d.id := $fffe;
        d.xpos := minz div 1024;
        d.xsize := 1;
        d.zpos := minx div 1024;
        d.zsize := (maxx - minx) div 1024;
      end;
      // pit
      if portal.normal.y = -1 then
      begin
        d.id := 4;
        d.xpos := minz div 1024;
        d.xsize := (maxz - minz) div 1024;
        d.zpos := minx div 1024;
        d.zsize := (maxx - minx) div 1024;
      end;
      // sky
      if portal.normal.y = 1 then
      begin
        d.id := $fffb;
        d.xpos := minz div 1024;
        d.xsize := (maxz - minz) div 1024;
        d.zpos := minx div 1024;
        d.zsize := (maxx - minx) div 1024;
      end;
      if not r.isFlipRoom then Inc(doorcount); // need to correct doorindex for flipped rooms later
      p.Rooms[i].doors[j] := d;
    end;  // loop thru portals
  end; //loop thru rooms

  // update number of things
  p.NumThings := doorcount;

  // determine slot value
  for i := 0 to High(rooms) do
  begin
    for j := 0 to High(rooms[i].Portals) do
    begin
      d := p.Rooms[i].doors[j];
      room := d.filler[0];
      for k := 0 to High(rooms[room].Portals) do
      begin
        if rooms[room].Portals[k].Adjoins(rooms[i].Portals[j],rooms[room].z,rooms[room].x,rooms[i].z,rooms[i].x) then
        begin
          p.Rooms[i].doors[j].slot := p.Rooms[room].doorthingindex[k];
          Break;
        end;
      end;
    end;
  end;

  // correct the doors for flipped rooms
  // the doors in the flip room must be the doors in non flip room
  for i := 0 to High(rooms) do
  begin
    if not rooms[i].isFlipRoom then Continue;
    r := rooms[i];
    for j := 0 to High(p.Rooms[i].doors) do
    begin
      d := p.Rooms[r.original_room].doors[j];
      p.Rooms[i].doorthingindex[j] := p.Rooms[r.original_room].doorthingindex[j];
      p.Rooms[i].doors[j].slot := d.slot; // may be unnecessary
      p.Rooms[i].doors[j].room := d.room;
    end;
  end;

  // fix floor and ceiling values for wall door blocks
  // the values used for door blocks must be the same as the
  // corresponding floor blocks in the other room which are
  // adjacent to the door in the other room
  // e.g.
  // floor adjacent room0   |s1|s2|s3|s4|
  // south door room0       |n1|n2|n3|n4|
  // north door room1       |s1|s2|s3|s4|
  // floor adjacent room1   |n1|n2|n3|n4|
  for i := 0 to High(p.Rooms) do
  begin
    if p.Rooms[i].id = 1 then Break;
    for j := 0 to High(p.Rooms[i].doors) do
    begin
      d := p.Rooms[i].doors[j];
      // exclude sky or pit doors
      if (d.id = 4) or (d.id = $fffb) then Continue;
      bloks := d.GetBlockIndices(p.Rooms[i].xsize);
      for k := 0 to High(p.Rooms[d.filler[0]].doors) do
      begin
        dd := p.Rooms[d.filler[0]].doors[k];
        found := (d.slot = p.Rooms[d.filler[0]].doorthingindex[k]);
        if found then Break;
      end;
      if not found then Continue;
      bloks2 := dd.GetAdjacentBlockIndices(p.Rooms[dd.room].xsize);
      if Length(bloks) <> Length(bloks2) then Continue;
      // fliprooms connected to fliprooms use those floor/ceiling values
      { TODO -oUsername -c : may need to same flip group 19/11/2020 4:48:23 PM }
      if p.Rooms[i].isFlipRoom and (p.Rooms[dd.room].fliproom > -1) then
        rm := p.Rooms[dd.room].fliproom
      else
        rm := dd.room;
      for k := 0 to High(bloks) do
      begin
        p.Rooms[i].blocks[bloks[k]].Floor := p.Rooms[rm].blocks[bloks2[k]].Floor;
        p.Rooms[i].blocks[bloks[k]].ceiling := p.Rooms[rm].blocks[bloks2[k]].ceiling;
      end;
    end;
  end;

  // toggle opacity -> no door floordata in tr4 but will have portal
  // need to mark these sectors as door blocks in prj since door created from portal
  // seems only wall door blocks need to be fixed
  // sky/pit door blocks fixed silently by ngle
  for i := 0 to High(p.Rooms) do
  begin
    if p.Rooms[i].id = 1 then Break;
    for j := 0 to High(p.Rooms[i].doors) do
    begin
      d := p.Rooms[i].doors[j];
      d.MarkDoorBlocks(p.Rooms[i]);
    end;
  end;

  // calculate room link value
  // making link = room number works (ngle doesn't freeze) but each room separate
  // Note: prj constructor sets link to room number
  // noticed that if ngle needs to repair map it automatically fixes links
  // set one link incorrect ($ffff) forces ngle to issue error msg and fix all links
  p.Rooms[0].link := $ffff;

  // aktrekker's method for reference
  if tr2prjlinks then
  begin
    previous_room := -1;
    for i := 0 to High(p.Rooms) do
    begin
      if p.Rooms[i].id = 1 then Break;
      p.Rooms[i].link := 0; // aktrekker initialises all links to 0
      if p.Rooms[i].numdoors = 0 then p.Rooms[i].link := i
      else
      begin
        if previous_room >= 0 then p.Rooms[previous_room].link := i
        else firstroom := i;
        previous_room := i;
      end;
    end;
    // last room links to first room with portals to complete "circle" of links
    if previous_room >= 0 then p.Rooms[previous_room].link := firstroom;
  end;
end;

function TPortalHelper.Adjoins(other: TPortal;x,z,x1,z1:Int32):Boolean;
var
  i : Integer;
  v, v1 : TVertex;
  minx,miny,minz,maxx,maxy,maxz : Integer;
  minx1,miny1,minz1,maxx1,maxy1,maxz1 : Integer;
begin
  if (Self.normal.x = -other.normal.x) and
     (Self.normal.y = -other.normal.y) and
     (Self.normal.z = -other.normal.z)
  then Result := True
  else Result := False;
  if Result = False then Exit;

  v := Self.vertices[0];
  v1 := other.vertices[0];
  minx := v.x; miny := v.y; minz := v.z;
  maxx := v.x; maxy := v.y; maxz := v.z;

  minx1 := v1.x; miny1 := v1.y; minz1 := v1.z;
  maxx1 := v1.x; maxy1 := v1.y; maxz1 := v1.z;

  // maybe should use trect instead

  for i := 1 to 3 do
  begin
    v := Self.vertices[i];
    v1 := other.vertices[i];
    if v.x < minx then minx := v.x;
    if v.y < miny then miny := v.y;
    if v.z < minz then minz := v.z;

    if v.x > maxx then maxx := v.x;
    if v.y > maxy then maxy := v.y;
    if v.z > maxz then maxz := v.z;

    if v1.x < minx1 then minx1 := v1.x;
    if v1.y < miny1 then miny1 := v1.y;
    if v1.z < minz1 then minz1 := v1.z;

    if v1.x > maxx1 then maxx1 := v1.x;
    if v1.y > maxy1 then maxy1 := v1.y;
    if v1.z > maxz1 then maxz1 := v1.z;
  end;
  // need to use absolute coordinates for x, z!!!!
  minx := minx + x; maxx := maxx + x;
  minz := minz + z; maxz := maxz + z;

  minx1 := minx1 + x1; maxx1 := maxx1 + x1;
  minz1 := minz1 + z1; maxz1 := maxz1 + z1;


  if Self.normal.z <> 0 then Result := (minx = minx1) and (miny = miny1) and
                             (maxx = maxx1) and (maxy = maxy1);
  if Self.normal.x <> 0 then Result := (miny = miny1) and (minz = minz1) and
                             (maxy = maxy1) and (maxz = maxz1);
  if Self.normal.y <> 0 then Result := (minx = minx1) and (minz = minz1) and
                             (maxx = maxx1) and (maxz = maxz1);

end;

function TPortalHelper.Equals(other: TPortal): Boolean;
var
  i : Integer;
  v, v1 : TVertex;
  minx,miny,minz,maxx,maxy,maxz : Integer;
  minx1,miny1,minz1,maxx1,maxy1,maxz1 : Integer;
begin
  if (Self.normal.x = other.normal.x) and
     (Self.normal.y = other.normal.y) and
     (Self.normal.z = other.normal.z) and
     (Self.toRoom = other.toRoom)
  then Result := True
  else Result := False;
  if Result = False then Exit;

  v := Self.vertices[0];
  v1 := other.vertices[0];
  minx := v.x; miny := v.y; minz := v.z;
  maxx := v.x; maxy := v.y; maxz := v.z;

  minx1 := v1.x; miny1 := v1.y; minz1 := v1.z;
  maxx1 := v1.x; maxy1 := v1.y; maxz1 := v1.z;

  for i := 1 to 3 do
  begin
    v := Self.vertices[i];
    v1 := other.vertices[i];
    if v.x < minx then minx := v.x;
    if v.y < miny then miny := v.y;
    if v.z < minz then minz := v.z;

    if v.x > maxx then maxx := v.x;
    if v.y > maxy then maxy := v.y;
    if v.z > maxz then maxz := v.z;

    if v1.x < minx1 then minx1 := v1.x;
    if v1.y < miny1 then miny1 := v1.y;
    if v1.z < minz1 then minz1 := v1.z;

    if v1.x > maxx1 then maxx1 := v1.x;
    if v1.y > maxy1 then maxy1 := v1.y;
    if v1.z > maxz1 then maxz1 := v1.z;
  end;

  Result := (minx = minx1) and (miny = miny1) and (minz = minz1) and
            (maxx = maxx1) and (maxy = maxy1) and (maxz = maxz1);
end;

end.

