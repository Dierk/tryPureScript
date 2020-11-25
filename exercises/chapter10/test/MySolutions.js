"use strict";

exports.volumeFn = function (l, w, h) {
  return l * w * h;
};

exports.volumeArrow = l => w => h =>
  l * w * h;

exports.cumulativeSumsComplex = arr => {
  let real = 0
  let imag = 0
  let sums = []
  arr.forEach(x => {
    real += x.real;
    imag += x.imag;
    sums.push({real: real, imag: imag});
  });
  return sums;
}

exports.quadraticRootsImpl = mkPair => poly => {
  let { a, b, c } = poly;
  let radicand = b * b - 4 * a * c;
  if (radicand >= 0) {
    let rt = Math.sqrt(radicand);
    return mkPair
      ({ real: (-b + rt) / (2 * a), imag: 0 })
      ({ real: (-b - rt) / (2 * a), imag: 0 });
  }else{
    let rt = Math.sqrt(-radicand);
    return mkPair
      ({ real: -b / (2 * a), imag: rt / (2 * a) })
      ({ real: -b / (2 * a), imag: -rt / (2 * a) });
  }
}

exports.valuesOfMapJson = j => {
  let m = new Map(j);
  return Array.from(m.values());
};

exports.quadraticRootsSetJson = poly => {
  let { a, b, c } = poly;
  let radicand = b * b - 4 * a * c;
  if (radicand >= 0) {
    let rt = Math.sqrt(radicand);
    return Array.from(new Set([
      { real: (-b + rt) / (2 * a), imag: 0 },
      { real: (-b - rt) / (2 * a), imag: 0 }]));
  }else{
    let rt = Math.sqrt(-radicand);
    return Array.from(new Set([
      { real: -b / (2 * a), imag: rt / (2 * a) },
      { real: -b / (2 * a), imag: -rt / (2 * a) }]));
  }
}

exports.quadraticRootsSafeJson = poly => {
  let { a, b, c } = poly;
  let radicand = b * b - 4 * a * c;
  if (radicand >= 0) {
    let rt = Math.sqrt(radicand);
    return [
      { real: (-b + rt) / (2 * a), imag: 0 },
      { real: (-b - rt) / (2 * a), imag: 0 }];
  }else{
    let rt = Math.sqrt(-radicand);
    return [
      { real: -b / (2 * a), imag: rt / (2 * a) },
      { real: -b / (2 * a), imag: -rt / (2 * a) }];
  }
}