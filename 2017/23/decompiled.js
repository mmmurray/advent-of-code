f = 1;
d = 2;
e = 2;

if (a === 0) {
  b = 67;
  c = 67;
} else {
  b = 106700;
  c = 123700;
}

function recurse() {
  g = d * e - b;

  if (g === 0) {
    f = 0;
  }

  e += 1;
  g = e - b;

  if (g !== 0) {
    recurse();
  }

  d += 1;
  g = d - b;

  if (g !== 0) {
    e = 2;
    recurse();
  }

  if (f === 0) {
    h += 1;
  }

  g = b - c;

  if (g === 0) {
    return;
  } else {
    b += 17;
    f = 1;
    d = 2;
    e = 2;
    recurse();
  }
}
