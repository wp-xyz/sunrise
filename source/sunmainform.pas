unit sunMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, EditBtn, ComCtrls, Grids, TAGraph, TASeries, TAIntervalSources,
  TAChartListbox, TATools, DateTimePicker, SpinEx, Types;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    btnCalc: TButton;
    btnClose: TButton;
    Chart: TChart;
    ChartListbox1: TChartListbox;
    ChartToolset: TChartToolset;
    ChartToolsetDataPointHintTool1: TDataPointHintTool;
    ChartToolsetPanDragTool1: TPanDragTool;
    ChartToolsetZoomDragTool1: TZoomDragTool;
    DateSource: TDateTimeIntervalChartSource;
    edDate: TDateTimePicker;
    seLatDeg: TFloatSpinEditEx;
    seLonDeg: TFloatSpinEditEx;
    TimeSource: TDateTimeIntervalChartSource;
    SeriesSunrise: TLineSeries;
    SeriesNoon: TLineSeries;
    SeriesSunset: TLineSeries;
    SeriesDayLength: TLineSeries;
    cbCity: TComboBox;
    cbDeltaUTC: TComboBox;
    gbLatitude: TGroupBox;
    gbLongitude: TGroupBox;
    gbCity: TGroupBox;
    gbDate: TGroupBox;
    gbTimeZone: TGroupBox;
    gbToday: TGroupBox;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblSunrise: TLabel;
    lblPlusMinusWeeks: TLabel;
    Label2: TLabel;
    lblLatDeg: TLabel;
    lblLonDeg: TLabel;
    lblNoon: TLabel;
    lblSunset: TLabel;
    PageControl1: TPageControl;
    pnlSunrise: TPanel;
    pnlNorthSouth: TPanel;
    pnlNoon: TPanel;
    pnlSunset: TPanel;
    pnlWestEast: TPanel;
    rbUTC: TRadioButton;
    rbLocalTime: TRadioButton;
    rbNorth: TRadioButton;
    rbWest: TRadioButton;
    rbSouth: TRadioButton;
    rbEast: TRadioButton;
    seNumWeeks: TSpinEdit;
    pgTable: TTabSheet;
    pgChart: TTabSheet;
    Grid: TStringGrid;
    procedure btnCloseClick(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
    procedure cbCityChange(Sender: TObject);
    procedure cbDeltaUTCChange(Sender: TObject);
    procedure ChartToolsetDataPointHintTool1Hint(ATool: TDataPointHintTool;
      const APoint: TPoint; var AHint: String);
    procedure ChartToolsetDataPointHintTool1HintLocation(
      ATool: TDataPointHintTool; AHintSize: TSize; var APoint: TPoint);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure rbLocalTimeChange(Sender: TObject);
    procedure rbUTCChange(Sender: TObject);
  private
    procedure AddLocation(AName: string; ALongitude, ALatitude, ADeltaUTC: Double);
    procedure BuildCityList;
    procedure BuildDeltaUTC;
    procedure Calculate;
    procedure ClearCityList;
    procedure ReadFromIni;
    procedure WriteToIni;

  public
    procedure BeforeRun;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  TypInfo, IniFiles,
  TACustomSeries,
  Solar;

//------------------------------------------------------------------------------
//   Utilities
//------------------------------------------------------------------------------

procedure BoldGroupbox(AGroupbox: TCustomGroupbox);
var
  i: Integer;
  propinfo: PPropInfo;
  cntrl: TControl;
  fnt: TFont;
begin
  for i:=0 to AGroupbox.ControlCount-1 do begin
    cntrl := AGroupbox.Controls[i];
    propinfo := GetPropInfo(cntrl, 'ParentFont');
    if propinfo <> nil then
      SetOrdProp(cntrl, propinfo, Longint(false));
    propinfo := GetPropInfo(cntrl, 'Font');
    if propinfo <> nil then begin
      fnt := TFont(GetObjectProp(cntrl, 'Font'));
      fnt.Style := [];
      SetObjectProp(cntrl, 'Font', fnt);
    end;
  end;
  AGroupbox.Font.Style := [fsBold];
end;


{ Requests painting of the headers of TCustomGroupbox descendants (TGroupbox,
  TRadiogroup, TCheckgroup) in bold. To be called from form or frame after
  construction with self as parameter. }
procedure BoldControl(AControl: TControl);
var
  i, n: Integer;
  s: String;
begin
  s := AControl.Name;
  if (AControl is TToolbar) then
    // skip all the toolbuttons
  else
  if (AControl is TCustomGroupbox) then
    BoldGroupbox(AControl as TCustomGroupbox)
  else begin
    n := AControl.ComponentCount;
    for i:=0 to AControl.ComponentCount-1 do
      if AControl.Components[i] is TControl then
        BoldControl(AControl.Components[i] as TControl)
  end;
end;

function CreateIni: TCustomIniFile;
var
  fn: String;
begin
  fn := ChangeFileExt(Application.ExeName, '.ini');
  Result := TIniFile.Create(fn);
end;


//------------------------------------------------------------------------------
//  TLocationList
//------------------------------------------------------------------------------

type
  TLocation = class
    Longitude, Latitude : extended;
    DeltaUTC : extended;
  end;
                 (*
  TLocationList = class(TStringList)
  public
    function  AddLocation(const ACity: String; ALatitude, ALongitude, ADeltaUTC: Double): integer;
    procedure Clear; override;
  end;


function TLocationList.AddLocation(const ACity:string;
  ALatitude, ALongitude, ADeltaUTC: Double): integer;
var
  L: TLocation;
begin
  L := TLocation.Create;
  L.Longitude := ALongitude;
  L.Latitude := ALatitude;
  L.DeltaUTC := ADeltaUTC;
  result := inherited AddObject(ACity, L);
end;

procedure TLocationList.Clear;
var
  i : integer;
begin
  for i:=Count-1 downto 0 do TLocation(Objects[i]).Free;
  inherited Clear;
end;             *)


//------------------------------------------------------------------------------
//  TMainForm
//------------------------------------------------------------------------------

procedure TMainForm.AddLocation(AName: string; ALongitude, ALatitude: Double;
  ADeltaUTC: Double);
var
  loc: TLocation;
begin
  loc := TLocation.Create;
  loc.Longitude := ALongitude;
  loc.Latitude := ALatitude;
  loc.DeltaUTC := ADeltaUTC;
  cbCity.Items.AddObject(AName, loc);
end;


procedure TMainForm.BeforeRun;
begin
  ReadFromIni;
end;


procedure TMainForm.btnCalcClick(Sender: TObject);
begin
  Calculate;
end;


procedure TMainForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;


procedure TMainForm.BuildCityList;
begin
  AddLocation('Munich',        48.136959,   -11.575899,        +1.0);
  AddLocation('Graz',          47+4.6/60.0, -(15+26.9/60.0),   +1.0);
  AddLocation('Berlin',        52.515526,   -13.377914,        +1.0);
  AddLocation('Dresden',       51.050409,   -13.737262,        +1.0);
  AddLocation('San Francisco', 37+46.0/60,   (122+25.0/60),    -8.0);
  AddLocation('Tokio',         35.683,      -139.767,          +9.0);
end;


procedure TMainForm.BuildDeltaUTC;
var
  L : TStringList;

  function utc(delta: Double) : TObject;
  begin
    result := TObject(PtrInt(round(delta*10)));
  end;
begin
  L := TStringList.Create;
  try
    L.AddObject('UTC-12',      utc(-12));
    L.AddObject('UTC-11',      utc(-11));
    L.AddObject('UTC-10',      utc(-10));
    L.AddObject('UTC-9',       utc(-9));
    L.AddObject('UTC-8 (PST)', utc(-8));
    L.AddObject('UTC-7 (MST)', utc(-7.0));
    L.AddObject('UTC-6 (CST)', utc(-6.0));
    L.AddObject('UTC-5',       utc(-5.0));
    L.AddObject('UTC-4',       utc(-4.0));
    L.AddObject('UTC-3',       utc(-3));
    L.AddObject('UTC-2',       utc(-2));
    L.AddObject('UTC-1',       utc(-1));
    L.AddObject('UTC (GMT)',   utc(0));
    L.AddObject('UTC+1 (MEZ)', utc(1));
    L.AddObject('UTC+2 (EET)', utc(2));
    L.AddObject('UTC+3',       utc(3));
    L.AddObject('UTC+4',       utc(4));
    L.AddObject('UTC+5',       utc(5));
    L.AddObject('UTC+6',       utc(6));
    L.AddObject('UTC+7',       utc(7));
    L.AddObject('UTC+8',       utc(8));
    L.AddObject('UTC+9',       utc(9));
    L.AddObject('UTC+10',      utc(10));
    L.AddObject('UTC+11',      utc(11));
    L.AddObject('UTC+12',      utc(12));
    CbDeltaUTC.Items.Assign(L);
  finally
    L.Free;
  end;
end;


procedure TMainForm.Calculate;
var
  longitude, latitude: Double;
  dateStart, dateEnd, date: TDateTime;
  sun: TSun;
  i: integer;
  deltaT: extended;
  srise: TTime;
  sset: TTime;
  snoon: TTime;
begin
  longitude := seLonDeg.Value; // + seLonMin.Value/60.0;
  if rbEast.Checked then longitude := -longitude;

  latitude := seLatDeg.Value; // + seLatMin.value/60.0;
  if rbSouth.Checked then latitude := -latitude;

  dateStart := edDate.Date - 7.0 * seNumWeeks.Value;
  dateEnd := edDate.Date + 7.0 * seNumWeeks.Value;

  Grid.RowCount := Grid.FixedRows + round(dateEnd - dateStart) + 1;

  SeriesSunset.Clear;
  SeriesNoon.Clear;
  SeriesSunrise.Clear;
  SeriesDaylength.Clear;

  if rbUTC.Checked then
    deltaT := 0.0
  else
    deltaT := integer(cbDeltaUTC.Items.Objects[cbDeltaUTC.ItemIndex]) / 240;

  try
    sun := SolarStuff(trunc(now), latitude, longitude);
  except
    exit;
  end;

  LblSunrise.Caption := TimeToStr(sun.SRise + deltaT);
  LblSunset.caption := TimeToStr(sun.SSet + deltaT);
  LblNoon.Caption := TimeToStr(sun.SNoon + deltaT);

  with Chart.BottomAxis do begin
    (*
    MinorTickCount := 6;
    Increment := DateTimeStep[dtOneWeek];
    if EdNumWeeks.AsInteger > 4 then begin
      Increment := DateTimeStep[dtOneMonth];
      MinorTickCount := 3;
    end;
    *)
  end;

  for i:=Grid.FixedRows to Grid.RowCount - 1 do begin
    date := DateStart + i - Grid.FixedRows;
    Grid.Cells[0, i] := DateToStr(date);
    try
      sun := SolarStuff(date, latitude, longitude);
      srise := sun.SRise;
      sset := sun.SSet;
      snoon := sun.SNoon;
      Grid.Cells[1, i] := TimeToStr(srise + deltaT);
      Grid.Cells[2, i] := TimeToStr(snoon + deltaT);
      Grid.Cells[3, i] := TimeToStr(sset + deltaT);
      Grid.Cells[4, i] := TimeToStr(sset - srise);
      SeriesSunrise.AddXY(date, frac(srise + deltaT));
      SeriesNoon.AddXY(date, frac(snoon + deltaT));
      SeriesSunset.AddXY(date, frac(sset + deltaT));
      SeriesDaylength.AddXY(date, sset - srise);
    except
      Grid.Cells[1, i] := '---';
      Grid.Cells[2, i] := '---';
      Grid.Cells[3, i] := '---';
      Grid.Cells[4, i] := '---';
    end;
  end;
end;


procedure TMainForm.cbCityChange(Sender: TObject);
var
  L: TLocation;
  i: integer;
  dUTC: Double;
begin
  L := TLocation(PTrInt(CbCity.Items.Objects[CbCity.ItemIndex]));

  seLonDeg.Value := abs(L.Longitude);  //trunc(abs(L.Longitude));
  //seLonMin.value := frac(abs(L.Longitude))*60.0;
  rbWest.Checked := L.Longitude > 0;
  rbEast.Checked := not RbWest.Checked;

  seLatDeg.Value := abs(L.Latitude); //trunc(abs(L.Latitude));
//  seLatMin.value := frac(abs(L.Latitude))*60.0;
  rbNorth.Checked := L.Latitude > 0;
  rbSouth.Checked := not RbNorth.Checked;

  for i := 0 to cbDeltaUTC.Items.Count-1 do begin
    dUTC := PtrInt(CbDeltaUTC.Items.Objects[i])/10.0;
    if (L.DeltaUTC = dUTC) then cbDeltaUTC.ItemIndex := i;
  end;

  Calculate;
end;

procedure TMainForm.cbDeltaUTCChange(Sender: TObject);
begin
  Calculate;
end;

procedure TMainForm.ChartToolsetDataPointHintTool1Hint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
var
  ser: TChartSeries;
  X: TDateTime;
begin
  if (ATool <> nil) and (ATool.Series is TChartSeries) then
  begin
    ser := TChartSeries(ATool.Series);
    X := ser.XValue[ATool.PointIndex];
    AHint := Format(
      '%s:' + LineEnding +
      '   Sunrise: %s' + LineEnding +
      '   Noon: %s' + LineEnding +
      '   Sunset: %s' + LineEnding +
      '   Day length: %s hours', [
      DateToStr(X),
      TimeToStr(SeriesSunrise.YValue[ATool.PointIndex]),
      TimeToStr(SeriesNoon.YValue[ATool.PointIndex]),
      TimeToStr(SeriesSunset.YValue[ATool.PointIndex]),
      TimeToStr(SeriesDayLength.YValue[ATool.PointIndex])
    ]);
  end else
    AHint := '';
end;

procedure TMainForm.ChartToolsetDataPointHintTool1HintLocation(
  ATool: TDataPointHintTool; AHintSize: TSize; var APoint: TPoint);
begin
  APoint.X := APoint.X + 10;
  APoint.Y := APoint.Y - AHintSize.CY;
end;

procedure TMainForm.ClearCityList;
var
  i: Integer;
  loc: TLocation;
begin
  for i := 0 to cbCity.Items.Count-1 do
  begin
    loc := TLocation(cbCity.Items.Objects[i]);
    loc.Free;
  end;
  cbCity.Items.Clear;
end;

procedure TMainForm.FormActivate(Sender: TObject);
begin
  Constraints.MinWidth := gbTimeZone.Left + gbTimeZone.Width + gbTimeZone.BorderSpacing.Right;
  Constraints.MaxWidth := gbTimeZone.Left + gbTimeZone.Width + gbTimeZone.BorderSpacing.Right;
  Constraints.MinHeight := (gbToday.Top + gbToday.Height) * 2;
  Width := 0;  // enforce constraints
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if CanClose then
    WriteToIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  BuildCityList;
  BuildDeltaUTC;
  edDate.Date := Date();
  BoldControl(self);
  lblSunrise.Caption := '---';
  lblNoon.Caption := '---';
  lblSunset.Caption := '---';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ClearCityList;
end;


procedure TMainForm.GridPrepareCanvas(sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
var
  ts: TTextStyle;
begin
  if aCol = 0 then begin
    ts := Grid.Canvas.TextStyle;
    ts.Alignment := taCenter;
    Grid.Canvas.TextStyle := ts;
  end;
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  W, H, L, T: Integer;
  R: TRect;
  List: TStringList;
  s, sname: String;
  loc: TLocation;
  sa: TStringArray;
  i: Integer;
  fs: TFormatSettings;
begin
  fs := FormatSettings;
  fs.DecimalSeparator := '.';

  ini := CreateIni;
  try
    R := Screen.WorkAreaRect;
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := Ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    if W > R.Width then W := R.Width;
    if H > R.Height then H := R.Height;
    if L + W > R.Right then L := R.Right - W;
    if L < 0 then L := 0;
    if T + H > R.bottom then T := R.Bottom - H;
    if T < 0 then T := 0;
    SetBounds(L, T, W, H);

    List := TStringList.Create;
    try
      ini.ReadSection('Locations', List);
      List.Sorted := true;
      ClearCityList;
      for i := 0 to List.Count-1 do
      begin
        sName := List[i];
        s := ini.ReadString('Locations', sname, '');
        if s <> '' then
        begin
          sa := s.Split(',');
          AddLocation(sName, StrToFloat(sa[0], fs), StrToFloat(sa[1], fs), StrToFloat(sa[2], fs));
        end;
      end;
    finally
      List.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.rbLocalTimeChange(Sender: TObject);
begin
  CbDeltaUTC.Enabled := RbLocalTime.Checked;
  Calculate;
end;

procedure TMainForm.rbUTCChange(Sender: TObject);
begin
  CbDeltaUTC.Enabled := RbLocalTime.Checked;
  Calculate;
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  i: Integer;
  loc: TLocation;
  s: String;
  fs: TFormatSettings;
begin
  fs := FormatSettings;
  fs.DecimalSeparator := '.';

  ini := CreateIni;
  try
    if WindowState = wsNormal then
    begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;
    for i := 0 to cbCity.Items.Count-1 do
    begin
      loc := TLocation(cbCity.Items.Objects[i]);
      s := Format('%.8f,%.8f,%.2f', [loc.Longitude, loc.Latitude, loc.DeltaUTC], fs);
      ini.WriteString('Locations', cbCity.Items[i], s);
    end;
  finally
    ini.Free;
  end;
end;

end.

