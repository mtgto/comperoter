var executor = {
    target: '',
    source: '',
    pos: 0,
    len: 0,
    program: '',
    stack: '',
    heap: '',
    labels: '',
    pc: 0,
    ERROR_EMPTY:{},
    ERROR_PARSE:{},
    ERROR_LABEL:{},
    output: '',

    execute: function(source, target) {
	this.target = target;
	this.ERROR_EMPTY = new Error('stack is empty');
	this.ERROR_PARSE = new Error('failed to parse');
	this.ERROR_LABEL = new Error('tried to jump to not defined label');
	var source = source.replace(/ﾍﾟﾛ/g, "0").replace(/ペロ/g, "1");
	while (source.indexOf(this.target) >= 0) {
	    source = source.replace(this.target, 'P');
	}
	this.source = source;
	this.pos = 0;
	this.len = this.source.length;
	this.labels = new Array();
	var program = this.parse();
	
	this.stack = new Array();
	this.heap = new Array();
	this.pc = 0;
	this.output = '';
	while (this.pc < program.length) {
	    console.log('pc='+this.pc);
	    console.log('stack='+this.stack);
	    console.log('heap='+this.heap);
	    console.log(program[this.pc]);
	    program[this.pc]();
	}
	return this.output;
    },
    parse: function() {
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
				throw executor.ERROR_EMPTY;
			    executor.stack.push(executor.stack[executor.stack.length-1]);
			    executor.pc++;
			}
		    }(this));
	    } else if (fst == 'P' && snd == '0' && trd == '1') {
		program.push(
		    function(executor){
			return function(){
			    if (executor.stack.length < 2)
				throw executor.ERROR_EMPTY;
			    var fst = executor.stack.pop();
			    var snd = executor.stack.pop();
			    executor.stack.push(fst);
			    executor.stack.push(snd);
			    executor.pc++;
			}
		    }(this));
	    } else if (fst == 'P' && snd == '0' && trd == '0') {
		program.push(
		    function(executor){
			return function(){
			    if (executor.stack.length < 1)
				throw executor.ERROR_EMPTY;
			    executor.stack.pop();
			    executor.pc++;
			}
		    }(this));
	    } else if (fst == '1' && snd == '0' && trd == 'P') {
		fth = this.read();
		if (fth == 'P') {
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 2)
				    throw executor.ERROR_EMPTY;
				var fst = executor.stack.pop();
				var snd = executor.stack.pop();
				executor.stack.push(snd+fst);
				executor.pc++;
			    }
			}(this));
		} else if (fth == '1') {
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 2)
				    throw executor.ERROR_EMPTY;
				var fst = executor.stack.pop();
				var snd = executor.stack.pop();
				executor.stack.push(snd-fst);
				executor.pc++;
			    }
			}(this));
		} else if (fth == '0') {
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 2)
				    throw executor.ERROR_EMPTY;
				var fst = executor.stack.pop();
				var snd = executor.stack.pop();
				executor.stack.push(snd*fst);
				executor.pc++;
			    }
			}(this));
		}
	    } else if (fst == '1' && snd == '0' && trd == '1') {
		fth = this.read();
		if (fth == 'P') {
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 2)
				    throw executor.ERROR_EMPTY;
				var fst = executor.stack.pop();
				var snd = executor.stack.pop();
				executor.stack.push(snd/fst);
				executor.pc++;
			    }
			}(this));
		} else if (fth == '1') {
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 2)
				    throw executor.ERROR_EMPTY;
				var fst = executor.stack.pop();
				var snd = executor.stack.pop();
				executor.stack.push(snd%fst);
				executor.pc++;
			    }
			}(this));
		} else {
		    throw new Exception(ERROR_PARSE);
		}
	    } else if (fst == '1' && snd == '1' && trd == 'P') {
		program.push(
		    function(executor){
			return function(){
			    if (executor.stack.length < 2)
				throw executor.ERROR_EMPTY;
			    var fst = executor.stack.pop();
			    var snd = executor.stack.pop();
			    executor.heap[Math.floor(fst)] = snd;
			    executor.pc++;
			}
		    }(this));
	    } else if (fst == '1' && snd == '1' && trd == '1') {
		program.push(
		    function(executor){
			return function(){
			    if (executor.stack.length < 1)
				throw executor.ERROR_EMPTY;
			    var num = executor.heap[Math.floor(executor.stack.pop())];
			    executor.stack.push(num);
			    executor.pc++;
			}
		    }(this));
	    } else if (fst == '0' && snd == 'P' && trd == 'P') {
		// label
		num = this.readNum();
		this.labels[num] = program.length;
	    } else if (fst == '0' && snd == 'P' && trd == '0') {
		// jump
		num = this.readNum();
		program.push(
		    function(executor, label){
			return function(){
			    if (typeof executor.labels[label] === 'undefined')
				throw executor.ERROR_LABEL;
			    executor.pc = executor.labels[label];
			}
		    }(this, num));
	    } else if (fst == '0' && snd == '1' && trd == 'P') {
		// jzero
		num = this.readNum();
		program.push(
		    function(executor, label){
			return function(){
			    if (executor.stack.length < 1)
				throw executor.ERROR_EMPTY;
			    if (typeof executor.labels[label] === 'undefined')
				throw executor.ERROR_LABEL;
			    if (executor.stack.pop() == 0)
				executor.pc = executor.labels[num];
			    else
				executor.pc++;
			}
		    }(this, num));
	    } else if (fst == '0' && snd == '1' && trd == '1') {
		// jneg
		num = this.readNum();
		program.push(
		    function(executor, label){
			return function(){
			    if (executor.stack.length < 1)
				throw executor.ERROR_EMPTY;
			    if (typeof executor.labels[label] === 'undefined')
				throw executor.ERROR_LABEL;
			    if (executor.stack.pop() < 0)
				executor.pc = executor.labels[label];
			    else
				executor.pc++;
			}
		    }(this, num));
	    } else if (fst == '0' && snd == '0' && trd == '0') {
		program.push(
		    function(executor){
			return function(){
			    executor.pc = 100000000;
			}
		    }(this, num));
	    } else if (fst == '1' && snd == 'P' && trd == 'P') {
		fth = this.read();
		if (fth == '0') {
		    // printChar
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 1)
				    throw executor.ERROR_EMPTY;
				executor.output += String.fromCharCode(executor.stack[executor.stack.length-1]);
				executor.pc++;
			    }
			}(this));
		} else if (fth == '1') {
		    // printNum
		    program.push(
			function(executor){
			    return function(){
				if (executor.stack.length < 1)
				    throw executor.ERROR_EMPTY;
				executor.output += executor.stack[executor.stack.length-1];
				executor.pc++;
			    }
			}(this));
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

