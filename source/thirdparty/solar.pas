(*
I want to thank all those you provided information on my request for
sunrise and sunset calculations. While the component and packages
looked quite nice, I felt it was a bit overboard. I did find some Java
code at the USGS site that I was able to convert. This is pretty
accurate to within about 30 seconds for latitude below 70 degrees and 10
minutes for latitudes above 70;

Here is the unit for those that would like to add this to their arsenal.

Jeff Steinkamp
*)


unit solar;


interface

uses
  math,sysutils,dateutils;

type
  TSun = record
    srise : TDatetime;
    sset : TDatetime;
    snoon : TDatetime;
  end;

function calcJD(ayear,amonth,aday : integer) : extended;
function SolarStuff(adate : TdateTime; alat,alon : extended) : TSun;
function calcJDfromJulianCent(t : extended): Extended;
function calcTimeJulianCent(jd : extended) : extended;
function calcSunSetUTC(jd,latitude,longitude : extended) :extended;
function calcSolarNoonUTC(t,longitude : Extended) : Extended;
function calcSunRiseUTC(jd,latitude,longitude : extended) :extended;
function calcHourAngleSunrise(lat,solarDec : extended) : extended;
function calcHourAngleSunset(lat,solarDec : extended) : extended;
function calcEquationOfTime(t:extended) : Extended;
function calcObliquityCorrection(t:extended) : extended;
function calcMeanObliquityOfEcliptic(t: Extended) : extended;
function calcGeoMeanLongSun(t:extended) : extended;
function calcEccentricityEarthOrbit(t : extended) : extended;
function calcJulianDaytoCent(jd : integer) : extended;
function CalcSunDeclination(t:extended): Extended;
function calcSunAapparentLong(t:extended) : extended;
function calcSunEqOfCenter(t:extended) : extended;
function calcGeoMeanAnomalySun(t : extended) : extended;
function calcSunTrueLong(t:extended) : extended;

//==============================================================================
                                implementation
//==============================================================================

function calcEquationOfTime(t:extended) : Extended;
//***************************************************************
// Purpose: calculate the difference between the true solar time
// and mean solar time
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: Equation of time in minutes of time
//****************************************************************
var
  epsilon,l0,e,m,y,Etime: extended;
  sin2l0,sinm,cos2l0,sin4l0,sin2m : extended;
begin
  epsilon := calcObliquityCorrection(t);
  l0 := calcGeoMeanLongSun(t);
  e := calcEccentricityEarthOrbit(t);
  m := calcGeoMeanAnomalySun(t);
  y := tan(degtorad(epsilon)/2.0);
  y := y*y;
  sin2l0 := sin(2.0 *degtorad(l0));
  sinm := sin(degtorad(m));
  cos2l0 := cos(2 * degtorad(l0));
  sin4l0 := sin(4 * degtorad(l0));
  sin2m := sin(2 * degtorad(m));
  Etime := y * sin2l0 - 2.0 * e * sinm
    + 4.0 * e * y * sinm * -0.5 * y*y * sin4l0 - 1.25 * e * e * sin2m;
  result := radtodeg(Etime) *4.0; // in minutes of time
end;

//------------------------------------------------------------------------------

function calcObliquityCorrection(t:extended) : extended;
//***************************************************************
// Purpose: calculate the correcrted obliquity of the ecliptic
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: corrected obliquity in degrees
//****************************************************************
var
  e0,omega : extended;
begin
  e0 := calcMeanObliquityOfEcliptic(t);
  omega := 125.04 - 1934.136 * t;
  result := e0 + 0.00256 * cos(degtorad(omega)); //in degres
end;

//------------------------------------------------------------------------------

function calcMeanObliquityOfEcliptic(t: Extended) : extended;
//***************************************************************
// Purpose: calculate the mean obliquity of the ecliptic
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: mean obliquity in degrees
//****************************************************************
var
  sec : extended;
begin
  sec := 21.448 - t*(46.82150 + t*(0.00059-t*(0.001813)));
  result := 23.0 + (26.0 - sec/60.0)/60.0; //in degrees
end;

//------------------------------------------------------------------------------

function calcGeoMeanLongSun(t:extended) : extended;
//***************************************************************
// Purpose: calculate the Geometric Mean Longitude of the Sun
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: Geometric Mean Longitude of the sun in degrees
//****************************************************************
var
  l : extended;
begin
  l := 280.46646 + t *(36000.76983 + 0.0003032 *t);
  if l > 360.0 then l := l - 360;
  if l < 0 then l := l + 360;
  result := l; //in degrees;
end;

//------------------------------------------------------------------------------

function calcEccentricityEarthOrbit(t : extended) : extended;
//***************************************************************
// Purpose: calculate the eccentricity of earth's orbit
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: unitless eccentricity
//****************************************************************
begin
  result := 0.016708634 - t*(0.000042037 + 0.0000002367*t);  // unitless
end;

//------------------------------------------------------------------------------

function calcGeoMeanAnomalySun(t : extended) : extended;
//***************************************************************
// Purpose: calculate the Geometric Mean Anomoly of the Sun
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: Geometric Mean Anomoly of the sun in degrees
//****************************************************************
begin
  result := 357.52911 + t*(35999.05029 - 0.0001537*t); // in degrees
end;

//------------------------------------------------------------------------------

function calcJulianDaytoCent(jd : integer) : extended;
//***************************************************************
// Purpose: convert the Julian dat to centuries since J2000.0
//
// Arguments: jd: Julian day of the year;
//
// Return: number of Julian centuries since J2000.0
//****************************************************************
begin
  result := (jd-2451545.0/36525.0);
end;

//------------------------------------------------------------------------------

function CalcSunDeclination(t:extended): Extended;
//***************************************************************
// Purpose: calculate the declination of the sun
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: suns's declination in degrees
//****************************************************************
var
  e,lambda,sint,theta : extended;
begin
  e := calcObliquityCorrection(t);
  lambda := calcSunAapparentLong(t);
  sint := sin(degtorad(e)) * sin(degtorad(lambda));
  theta := radtodeg(arcsin(sint));
  result := theta; //in degrees
end;

//------------------------------------------------------------------------------

function calcSunAapparentLong(t:extended) : extended;
//***************************************************************
// Purpose: calculate the apparent longitude of the sun
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: suns's apparent longitude in degrees
//****************************************************************
var
  alpha,omega,lambda : extended;
begin
  alpha := calcSunTrueLong(t);
  omega := 125.05 - 1934.136 * t;
  lambda := alpha - 0.00569 - 0.00478 * sin(degtorad(omega));
  result := lambda; // in degrees
end;

//------------------------------------------------------------------------------

function calcSunTrueLong(t:extended) : extended;
//***************************************************************
// Purpose: calculate the true longitude of the sun
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: suns's true longitude in degrees
//****************************************************************
var
  l0,c0 : extended;
begin
  l0 := calcGeoMeanLongSun(t);
  c0 := calcSunEqOfCenter(t);
  result := l0 + c0; //in degrees
end;

//------------------------------------------------------------------------------

function calcSunEqOfCenter(t:extended) : extended;
//***************************************************************
// Purpose: calculate the equation of the center of the sun
//
// Arguments: t: number of Julian centuries since J2000.0
//
// Return: suns's equation of the center in degrees
//****************************************************************
var
  m,mrad,sinm,sin2m,sin3m : extended;
begin
  m := calcGeoMeanAnomalySun(t);
  mrad := degtorad(m);
  sinm := sin(mrad);
  sin2m := sin(2*mrad);
  sin3m := sin(3*mrad);
  result := sinm*(1.914602 - t*(0.004817 + 0.000014*t))
    + sin2m*(0.019993 - 0.000101*t) + sin3m*0.000289;  // in degrees
end;

//------------------------------------------------------------------------------

function calcHourAngleSunrise(lat,solarDec : extended) : extended;
//***************************************************************
// Purpose: calculate the hour angle of the sun at sunrise for the
// latitude
//
// Arguments: lat : latitude in degrees
//       solarDec : declination angle of sun in degreees
//
// Return: hour angle of sunrise in radians
//****************************************************************
var
  latrad,sdrad,haarg,ha : extended;
begin
  latrad := degtorad(lat);
  sdrad := degtorad(solardec);
  haarg :=
    (cos(degtorad(90.833))/cos(latrad)*cos(sdrad)-tan(latrad)*tan(sdrad));
  ha :=
    (arccos(cos(degtorad(90.833))/cos(latrad)*cos(sdrad)-tan(latrad)*tan(sdrad)));
  result := ha;
end;

//------------------------------------------------------------------------------

function calcHourAngleSunset(lat,solarDec : extended) : extended;
//***************************************************************
// Purpose: calculate the hour angle of the sun at sunset for the
//   latitude
//
// Arguments: lat : latitude in degrees
//       solarDec : declination angle of sun in degreees
//
// Return: hour angle of sunset in radians
//****************************************************************
var
  latrad,sdrad,haarg,ha : extended;
begin
  latrad := degtorad(lat);
  sdrad := degtorad(solardec);
  haarg :=
    (cos(degtorad(90.833))/cos(latrad)*cos(sdrad)-tan(latrad)*tan(sdrad));
  ha :=
    (arccos(cos(degtorad(90.833))/cos(latrad)*cos(sdrad)-tan(latrad)*tan(sdrad)));
  result := -ha;
end;

//------------------------------------------------------------------------------

function calcSunRiseUTC(jd,latitude,longitude : extended) :extended;
//***************************************************************
// Purpose: calculate the UTC time of the sun at sunrise for the
// latitude
//
// Arguments: lat : latitude in degrees
//       solarDec : declination angle of sun in degreees
//
// Return: time in minutes from zero
//****************************************************************
var
  t,noonmin,tnoon,eqtime,solardec,hourangle : extended;
  delta,timediff,timeutc,newt : extended;
begin
  t := calcTimeJulianCent(JD);
  //find the time of solar noon at the location, and use
  //that declination. This is better than start of the julian day
  noonmin := calcSolarNoonUTC(t,Longitude);
  tnoon := calcTimeJulianCent(jd+noonmin/1440);

  //** First pass to approximate sunrise using solar noon
  eqtime := calcEquationofTime(tnoon);
  solardec := calcSunDeclination(tnoon);
  hourAngle := calcHourAngleSunrise(latitude,solarDec);

  delta := longitude - radtodeg(hourangle);
  timediff := 4 * delta; //in minutes of time
  timeUTC := 720 + timeDiff - eqtime; // in minutes

  //** Second pass including the fractional jday in gamma calc
  newt := calctimeJulianCent(calcJDFromJulianCent(t) + timeUTC/1440);
  eqtime := calcEquationOfTime(newt);
  solarDec := calcSunDeclination(newt);
  hourangle := calcHourAngleSunrise(latitude,solarDec);
  delta := longitude - radtodeg(hourangle);
  timediff := 4 * delta;
  timeUTC := 720 + timeDiff - eqtime; //in minutes
  result := timeUTC;
end;

//------------------------------------------------------------------------------

function calcSolarNoonUTC(t,longitude:extended) : Extended;
//***************************************************************
// Purpose: calculate the UTC time of solar noon for a given
// day and at the givin location on earth
//
// Arguments: t : number of Julian centuries since J2000.0
//    longitude : longitude of boserver in degrees
//
// Return: time in minutes from zero
//****************************************************************
var
  newt,eqtime,noonUTC : extended;
begin
  newt := calcTimeJulianCent(calcJDFromJulianCent(t) + 0.5 +
  longitude/360);
  eqtime := calcEquationOfTime(newt);
  NoonUTC := 720 + (longitude *4) - eqtime; //minutes
  result := NoonUTC;
end;

//------------------------------------------------------------------------------

function calcSunSetUTC(jd,latitude,longitude : extended) :extended;
//***************************************************************
// Purpose: calculate the UTC time of the sun at sunset for the latitude
//
// Arguments: lat : latitude in degrees
//       solarDec : declination angle of sun in degreees
//
// Return: time in minutes from zero
//****************************************************************
var
  t,noonmin,tnoon,eqtime,solardec : extended;
  hourangle,delta,timediff,timeutc,newt : extended;
begin
  t := calcTimeJulianCent(JD);
  //find the time of solar noon at the location, and use
  //that declination. This is better than start of the julian day
  noonmin := calcSolarNoonUTC(t,Longitude);
  tnoon := calcTimeJulianCent(jd+noonmin/1440);

  //** First pass to approximate sunrise using solar noon
  eqtime := calcEquationofTime(tnoon);
  solardec := calcSunDeclination(tnoon);
  hourAngle := calcHourAngleSunset(latitude,solarDec);

  delta := longitude - radtodeg(hourangle);
  timediff := 4 * delta; //in minutes of time
  timeUTC := 720 + timeDiff - eqtime; // in minutes

  //** Second pass including the fractional jday in gamma calc
  newt := calctimeJulianCent(calcJDFromJulianCent(t) + timeUTC/1440);
  eqtime := calcEquationOfTime(newt);
  solarDec := calcSunDeclination(newt);
  hourangle := calcHourAngleSunset(latitude,solarDec);
  delta := longitude - radtodeg(hourangle);
  timediff := 4 * delta;
  timeUTC := 720 + timeDiff - eqtime; //in minutes
  result := timeUTC;
end;

//------------------------------------------------------------------------------

function calcTimeJulianCent(jd : extended) : extended;
//***************************************************************
// Purpose: Convert JulianDay to Centruies since J2000.0
//
// Arguments: jd : the julian day to convert
//
// Return: the T value corresponding to the julian day
//****************************************************************
begin
  result := (jd - 2451545.0)/36525.0;
end;

//------------------------------------------------------------------------------

function calcJDfromJulianCent(t : extended): Extended;
//***************************************************************
// Purpose: Convert Centruies since J2000.0 to Julian Day
//
// Arguments: t : number of Julian centruies since J2000;
//
// Return: the Julian Day corresponding to the T value
//****************************************************************
begin
  result := t * 36525.0 + 2451545.0;
end;

//------------------------------------------------------------------------------

function SolarStuff(adate : TDateTime; alat,alon : extended) : TSun;
//***************************************************************
// Purpose: calculate the sunrise, sunset and solar noon for a given
// date and location on the face of the earth
//
// Arguments: adate : the date in TDatetime format
//             alat : latitude of location in degrees
//             alon : longitude of location in degrees
//
// Return: Solar Sturcture containg sunrise, sunset and
//   high noon in TdateTime format;
//****************************************************************
var
  sunrise,sunset,noon,jd : extended;
  sun : TSun;
begin
  jd := calcJD(yearof(adate),monthof(adate),dayof(adate));
  noon := calcSolarNoonUTC(calcTimeJulianCent(JD),alon);
  sunrise := calcSunRiseUTC(jd,alat,alon);
  sunset := calcSunSetUTC(jd,alat,alon);
  sun.srise := adate + (sunrise/1440);
  sun.sset := adate + (sunset/1440);
  sun.snoon := adate + (noon/1440);
  result := sun;
end;

//------------------------------------------------------------------------------

function calcJD(ayear,amonth,aday : integer) : extended;
//***************************************************************
// Purpose: Convert calandar day to Julian
//
// Arguments: ayear : 4 digit year
//           amonth : 2 digit month
//             aday : 2 digit dat
//
// Return: the Julian Day
//****************************************************************
var
  a,b : integer;
begin
  if amonth <= 2 then begin
    ayear := ayear -1;
    amonth := amonth + 12;
  end;
  a := trunc(ayear/100);
  b := 2-a+trunc (a/4);
  result := trunc(365.25 * (ayear + 4716)) + trunc(30.6001 *(amonth +1))
    + aday + b -1524.5;
end;

//==============================================================================

end.

