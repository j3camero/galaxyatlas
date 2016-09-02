
function Sigmoid(x) {
    return 1.0 / (1.0 + Math.exp(x));
}

function MagicFormula(s, w) {
    var a = Math.log(s) - Math.log(w);
    var b = Sigmoid(-2.03935397 * a);
    var c = Sigmoid(-3.065390091 * a + 1.851112427);
    var model = (b + 1.872968808 * c * (1.0 - c)) / (s * s);
}
