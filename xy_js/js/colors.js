// hue in [0,360) saturation=1, value=1
function hsvToRGB(hue, saturation, value) {
  saturation = saturation || 1;
  value = value || 1;

  var hi;
  var f;
  var p;
  var q;
  var t;
  
  while (hue < 0) {
    hue += 360;
  }
  hue = hue % 360;
  
  saturation = saturation < 0 ? 0
    : saturation > 1 ? 1
    : saturation;
  
  value = value < 0 ? 0
    : value > 1 ? 1
    : value;

  value *= 255;
  hi = (hue / 60 | 0) % 6;
  f = hue / 60 - hi;
  p = value * (1 -           saturation) | 0;
  q = value * (1 -      f  * saturation) | 0;
  t = value * (1 - (1 - f) * saturation) | 0;
  value |= 0;

  switch (hi) {
  case 0:
    return [value, t, p];
  case 1:
    return [q, value, p];
  case 2:
    return [p, value, t];
  case 3:
    return [p, q, value];
  case 4:
    return [t, p, value];
  case 5:
    return [value, p, q];
  }

  throw new Error('invalid hue');
}
