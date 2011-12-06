var executor = {
    target: "",
    source: "",
    pos: 0,
    len: 0,
    program: "",
    stack: "",
    heap: "",
    labels: "",
    pc: 0,
    
    execute: function(source, target) {
	this.target = target;
	var source = source.replace(/ﾍﾟﾛ/g, "0").replace(/ペロ/g, "1");
	while (source.indexOf(this.target) >= 0) {
	    source = source.replace(this.target, 'P');
	}
	this.source = source;
	this.pos = 0;
	this.len = this.source.length;
	this.stack = new Array();
	this.heap = new Array();
	this.labels = new Array();
	this.pc = 0;
	var program = this.parse();
	console.log(program[0]);
	console.log(program[0]());
	/*
	while (this.pc < program.length) {
	    program[this.pc]();
	}
	 */
    },
    parse: function() {
	const ERROR_EMPTY = 'stack is empty';
	const ERROR_PARSE = 'failed to parse';
	var program = new Array();
	while (this.pos < this.len) {
	    var fst = this.read();
	    var snd = this.read();
	    var trd;
	    var fth;
	    if (fst == 'P' && snd == '1') {
		var num = this.readNum();
		program.push(
		    function(executor, num){
			return function(){
			    executor.stack.push(num);
			    executor.pc++;
			};
		    }(this, num));
		continue;
	    }
	    trd = this.read();
	    if (fst == 'P' && snd == '0' && trd == 'P') {
		program.push(
		    function(executor){
			return function(){
			    if (executor.stack.length == 0)
				throw new Exception(ERROR_EMPTY);
			    executor.stack.push(executor.stack[executor.stack.length-1]);
			    executor.pc++;
			}
		    }(this));
	    } else if (fst == 'P' && snd == '0' && trd == '1') {
		program.push(
		    function(){
			if (this.stack.length < 2)
			    throw new Exception(ERROR_EMPTY);
			var fst = this.stack.pop();
			var snd = this.stack.pop();
			this.stack.push(fst);
			this.stack.push(snd);
			this.pc++;
		    }
		);
	    } else if (fst == 'P' && snd == '0' && trd == '0') {
		program.push(
		    function(){
			if (this.stack.length < 1)
			    throw new Exception(ERROR_EMPTY);
			this.stack.pop();
			this.pc++;
		    }
		)
	    } else if (fst == '1' && snd == '0' && trd == 'P') {
		fth = this.read();
		if (fth == 'P') {
		    program.push(
			function(){
			    if (this.stack.length < 2)
				throw new Exception(ERROR_EMPTY);
			    var fst = this.stack.pop();
			    var snd = this.stack.pop();
			    this.stack.push(snd+fst);
			    this.pc++;
			}
		    );
		} else if (fth == '1') {
		    program.push(
			function(){
			    if (this.stack.length < 2)
				throw new Exception(ERROR_EMPTY);
			    var fst = this.stack.pop();
			    var snd = this.stack.pop();
			    this.stack.push(snd-fst);
			    this.pc++;
			}
		    );
		} else if (fth == '0') {
		    program.push(
			function(){
			    if (this.stack.length < 2)
				throw new Exception(ERROR_EMPTY);
			    var fst = this.stack.pop();
			    var snd = this.stack.pop();
			    this.stack.push(snd*fst);
			    this.pc++;
			}
		    );
		}
	    } else if (fst == '1' && snd == '0' && trd == '1') {
		fth = this.read();
		if (fth == 'P') {
		    program.push(
			function(){
			    if (this.stack.length < 2)
				throw new Exception(ERROR_EMPTY);
			    var fst = this.stack.pop();
			    var snd = this.stack.pop();
			    this.stack.push(snd/fst);
			    this.pc++;
			}
		    );
		} else if (fth == '1') {
		    program.push(
			function(){
			    if (this.stack.length < 2)
				throw new Exception(ERROR_EMPTY);
			    var fst = this.stack.pop();
			    var snd = this.stack.pop();
			    this.stack.push(snd%fst);
			    this.pc++;
			}
		    );
		} else {
		    throw new Exception(ERROR_PARSE);
		}
	    } else if (fst == '1' && snd == '1' && trd == 'P') {
		program.push(
		    function(){
			if (this.stack.length < 2)
			    throw new Exception(ERROR_EMPTY);
			var fst = this.stack.pop();
			var snd = this.stack.pop();
			this.heap[Math.floor(fst)] = snd;
			this.pc++;
		    }
		);
	    } else if (fst == '1' && snd == '1' && trd == '1') {
		program.push(
		    function(){
			if (this.stack.length < 1)
			    throw new Exception(ERROR_EMPTY);
			var num = this.heap[Math.floor(this.stack.pop())];
			this.stack.push(num);
			this.pc++;
		    }
		);
	    } else if (fst == '0' && snd == 'P' && trd == 'P') {
		// label
		num = this.readNum();
		labels[num] = program.length;
	    } else if (fst == '0' && snd == 'P' && trd == '0') {
		// jump
		num = this.readNum();
		program.push(
		    function(){
			this.pc = this.labels[num];
		    }
		);
	    } else if (fst == '0' && snd == '1' && trd == 'P') {
		// jzero
		num = this.readNum();
		program.push(
		    function(){
			if (this.stack.length < 1)
			    throw new Exception(ERROR_EMPTY);
			if (this.stack.pop() == 0)
			    this.pc = this.labels[num];
			else
			    this.pc++;
		    }
		);
	    } else if (fst == '0' && snd == '1' && trd == '1') {
		// jneg
		num = this.readNum();
		program.push(
		    function(){
			if (this.stack.length < 1)
			    throw new Exception(ERROR_EMPTY);
			if (this.stack.pop() < 0)
			    this.pc = this.labels[num];
			else
			    this.pc++;
		    }
		);
	    } else if (fst == '1' && snd == 'P' && trd == 'P') {
		fth = this.read();
		if (fth == '0') {
		    // printChar
		    program.push(
			function(){
			    if (this.stack.length < 1)
				throw new Exception(ERROR_EMPTY);
			    console.log(String.fromCharCode(this.stack.pop()));
			    this.pc++;
			}
		    );
		} else if (fth == '1') {
		    // printNum
		    program.push(
			function(){
			    if (this.stack.length < 1)
				throw new Exception(ERROR_EMPTY);
			    console.log(this.stack.pop());
			    this.pc++;
			}
		    );
		} else {
		    throw new Exception(ERROR_PARSE);
		}
	    } else if (fst == '1' && snd == 'P' && trd == '1') {
		fth = this.read();
		if (fth == 'P') {
		    // readChar
		} else if (fth == '0') {
		    // readNum
		} else {
		    throw new Exception(ERROR_PARSE);
		}
	    }
	}
	return program;
    },
    read: function() {
	if (this.pos >= this.len)
	    throw new Exception("reached program end");
	var ch = this.source[this.pos];
	this.pos++;
	return ch;
    },
    readNum: function() {
	var num = 0;
	while (true) {
	    switch (this.read()) {
		case '0': num = num * 2; break;
		case '1': num = num * 2 + 1; break;
		case 'P': return num;
	    }
	}
    }
}

