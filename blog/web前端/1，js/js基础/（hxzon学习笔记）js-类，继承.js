//（hxzon学习笔记）js-类，继承

//by hxzon
//《js架构\Spine\（hxzon学习笔记）《JavaScript Web Applications》面向对象（类，继承）》
//=======
//1，创建类

var Class = function(){
	var klass = function(){
		this.init.apply(this, arguments);
	};
	klass.prototype.init = function(){};
	return klass;
};

var Person = new Class;
Person.prototype.init = function(){
	// Called on Person instantiation
};
// Usage:
var person = new Person;

//====
//2，
//添加类函数：

Person.find = function(id){ /*...*/ };
var person = Person.find(1);

//添加实例函数：

Person.fn = Person.prototype;//取个别名，便于理解

Person.fn.run = function(){ /*...*/ };

var person = new Person;
person.run();

//====
//3，工具方法

var Class = function(){
	var klass = function(){
		this.init.apply(this, arguments);
	};
	klass.prototype.init = function(){};
	// Shortcut to access prototype
	klass.fn = klass.prototype;
	// Shortcut to access class
	klass.fn.parent = klass;
	// Adding class properties
	klass.extend = function(obj){
		var extended = obj.extended;
		for(var i in obj){
			klass[i] = obj[i];
		}
		if (extended) extended(klass);
	};
	// Adding instance properties
	
	klass.include = function(obj){
		var included = obj.included;
		for(var i in obj){
			klass.fn[i] = obj[i];
		}
		if (included) included(klass);
	};
	return klass;
};

//示例：extend() 添加类方法，include() 添加实例方法。
var Person = new Class;
Person.extend({
	find: function(id) { /* ... */ },
	exists: functions(id) { /* ... */ }
});
var person = Person.find(1);

var Person = new Class;
Person.include({
	save: function(id) { /* ... */ },
	destroy: functions(id) { /* ... */ }
});
var person = new Person;
person.save();

//我们也因此获得模块的能力。
//模块即可复用的代码。

//示例：
var ORMModule = {
	save: function(){
		// Shared function
	}
};

var Person = new Class;
var Asset = new Class;
Person.include(ORMModule);
Asset.include(ORMModule);

//==========
//4，继承

var Class = function(parent){
	var klass = function(){
		this.init.apply(this, arguments);
	};
	// Change klass' prototype 
	if (parent) {
		var subclass = function() { };
		subclass.prototype = parent.prototype;
		klass.prototype = new subclass;
	};
	klass.prototype.init = function(){};
	// Shortcuts
	klass.fn = klass.prototype;
	klass.fn.parent = klass;
	klass._super = klass.__proto__; 
	/* include/extend code... */
	return klass;
};

//示例：
var Animal = new Class;
Animal.include({
	breath: function(){ 
		console.log('breath'); 
	}
});

var Cat = new Class(Animal)

// Usage
var tommy = new Cat;
tommy.breath();

//====
//5，方法调用

//改变上下文。

function.apply(this, [1, 2, 3])
function.call(this, 1, 2, 3);

var Class = function(parent){ 
	var klass = function(){
		this.init.apply(this, arguments);
	};
	klass.prototype.init = function(){};
	klass.fn = klass.prototype;
	// Adding a proxy function
	klass.proxy = function(func){
		var self = this;
		return(function(){ 
			return func.apply(self, arguments); 
		});
	}
	// Add the function on instances too
	klass.fn.proxy = klass.proxy;
	return klass;
};

//示例：
var Button = new Class;
Button.include({
	init: function(element){
		this.element = jQuery(element);
		// Proxy the click function
		this.element.click(this.proxy(this.click));//代理
	},
	click: function(){ /* ... */ }
});

//es5增加了bind()函数。

Button.include({
	init: function(element){
		this.element = jQuery(element);
		// Bind the click function
		this.element.click(this.click.bind(this));
	},
	click: function(){ /* ... */ }
});

//可以用以下代码，来兼容旧浏览器。

if ( !Function.prototype.bind ) {
	Function.prototype.bind = function( obj ) {
		var slice = [].slice,
		args = slice.call(arguments, 1), 
		self = this, 
		nop = function () {}, 
		bound = function () {
			return self.apply( this instanceof nop ? this : ( obj || {} ), 
			args.concat( slice.call(arguments) ) ); 
		};
		nop.prototype = self.prototype;
		bound.prototype = new nop();
		return bound;
	};
}

//==========
//6，完整代码

var Class = function(parent){
	var klass = function(){
		this.init.apply(this, arguments);
	};
	// Change klass' prototype 
	if (parent) {
		var subclass = function() { };
		subclass.prototype = parent.prototype;
		klass.prototype = new subclass;
	};
	klass.prototype.init = function(){};
	// Shortcuts
	klass.fn = klass.prototype;
	klass.fn.parent = klass;
	klass._super = klass.__proto__; 

	// Adding class properties
	klass.extend = function(obj){
		var extended = obj.extended;
		for(var i in obj){
			klass[i] = obj[i];
		}
		if (extended) extended(klass);
	};

	// Adding instance properties
	klass.include = function(obj){
		var included = obj.included;
		for(var i in obj){
			klass.fn[i] = obj[i];
		}
		if (included) included(klass);
	};

	klass.proxy = function(func){
		var self = this;
		return(function(){ 
			return func.apply(self, arguments); 
		});
	}
	// Add the function on instances too
	klass.fn.proxy = klass.proxy;

	return klass;
};


