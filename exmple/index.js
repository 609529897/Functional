var IO = function (f) {
    this.__value = f;
}

IO.of = x => new IO(() => x)

IO.prototype.map = f => new IO(_.compose(f, this.__value))