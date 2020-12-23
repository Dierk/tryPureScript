"use strict";

exports.volumeFn = function (length, width, height) {
    return length * width * height;
}

exports.volumeArrow = length => width => height =>
    length * width * height;

const addComplex = a => b => {
    return {
        real: a.real + b.real,
        imag: a.imag + b.imag
    }
}

exports.cumulativeSumsComplex = xs => {
    let cumSum = { real: 0.0, imag: 0.0 }
    return xs.map(c => {
        cumSum = addComplex(cumSum)(c);
        return cumSum;
    });
}

exports.quadraticRootsImpl = pairConstructor => quadratic => {
    let { a, b, c } = quadratic;
    let rad = b * b - 4 * a * c;
    if (rad >= 0) {
      let rt = Math.sqrt(rad);
      return pairConstructor
        ({ real: (-b + rt) / (2 * a), imag: 0 })
        ({ real: (-b - rt) / (2 * a), imag: 0 });
    } else {
      let rt = Math.sqrt(-rad);
      return pairConstructor
        ({ real: -b / (2 * a), imag: rt / (2 * a) })
        ({ real: -b / (2 * a), imag: -rt / (2 * a) });
    }
};

exports.valuesOfMapImpl = json => {
    let m = new Map(json);
    return Array.from(new Set(m.values()));
}

exports.quadraticRootsSetImpl = quadratic => {
    let { a, b, c } = quadratic;
    let rad = b * b - 4 * a * c;
    if (rad >= 0) {
        let rt = Math.sqrt(rad);
        return Array.from(new Set([
            { real: (-b + rt) / (2 * a), imag: 0 },
            { real: (-b - rt) / (2 * a), imag: 0 }]));
    } else {
        let rt = Math.sqrt(-rad);
        return Array.from(new Set([
            { real: -b / (2 * a), imag: rt / (2 * a) },
            { real: -b / (2 * a), imag: -rt / (2 * a) }]));
    }
};

exports.quadraticRootsSafeImpl = quadratic => {
    let { a, b, c } = quadratic;
    let rad = b * b - 4 * a * c;
    if (rad >= 0) {
      let rt = Math.sqrt(rad);
      return [
        { real: (-b + rt) / (2 * a), imag: 0 },
        { real: (-b - rt) / (2 * a), imag: 0 }];
    } else {
      let rt = Math.sqrt(-rad);
      return [
        { real: -b / (2 * a), imag: rt / (2 * a) },
        { real: -b / (2 * a), imag: -rt / (2 * a) }];
    }
  };