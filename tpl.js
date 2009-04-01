pathresolve = function(path, o) {
    if(!path) return o
    var p = path.split('.')
    var t = p.shift()
    try {
    if(t.indexOf('[') >= 0) {
        var m = t.match(/\[["']?([0-9A-Za-z_]+)["']?\]/g)
        var t = t.replace(/\[["']?[[0-9A-Za-z_]+["']?\]/g, '')
        var o = o[t]
        for(var i=0; i<m.length; i++) {
            k = m[i].replace(/[\[\]'"]+/g, '')
            if(o && o[k] !== undefined && o[k] !== null) o = o[k]
            else return o
        }
        return pathresolve(p.join('.'), o)
    }
    if(o !== undefined && o[t] !== null) {
        var r = pathresolve(p.join('.'), o[t])
    } else {
        var r = null //beware of storing nulls, since we're using null to mean 'no tengo'
    }
    } catch(e) {
        var r = null;
    }
    return r
}
tpl = {};
tpl.render=  function(src, c) {
    var t = new tpl.t(src, c);
    return t.render();
}
tpl.t = function(src, ctxt, o) {
    this._config = this._defaults || {};
    var o = o || {};
    for(k in o) this._config[k] = o[k];
    this._src = src;
    this._ctxt = ctxt;
};
tpl.t.prototype = {
    L: '<',
    R: '>',
    D: '%',
    _tags: {
        'FOR': { 
            pattern: /^for\s+([A-Za-z]\w*)\s*,\s*([A-Za-z]\w*)\s+in\s+([A-Za-z][A-Za-z0-9_\]\[.]*)$/,
            'do_': function(c, tlist, ctxt) {
                var closed = false;
                for(var i=0, forr=0; i<tlist.length; i++) {
                    if(tlist[i][0] == "FOR") forr++;
                    else if(tlist[i][0] == "ENDFOR" && !forr) {
                        closed = true;
                        break;
                    } else if(tlist[i][0] == "ENDFOR") forr--;
                }
                if(!closed) {
                    throw new Error('No end tag for FOR @ '+c[1].line+':'+c[1].offset+'.');
                    return false;
                }
                var doblock = tlist.slice(0,i);
                var kn = c[2], vn = c[3], p = c[4];
                var ctxt_ = pathresolve(p, ctxt);
                var done = "";
                var iterctxt = {};
                for(var k in ctxt) { iterctxt[k] = ctxt[k]; }
                if(ctxt_ !== undefined) {
                    for(var k in ctxt_) {
                        iterctxt[kn] = k;
                        iterctxt[vn] = ctxt_[k];
                        done += this.evaluate(doblock, iterctxt);
                    }
                }
                return done+this.evaluate(tlist.slice(i), ctxt);
            }
        },
        'ENDFOR': {
            pattern: /^endfor$/,
            'do_': function(c, tlist, ctxt) {
                return this.evaluate(tlist, ctxt);    
            }
        },
        'IF': { 
            pattern: /^if\s+(.*)$/,
            'do_': function(c, tlist, ctxt) {
                var closed = false;
                var elsse = false;
                for(var i=0, iff=0; i<tlist.length; i++) {
                    if(tlist[i][0] == 'IF') iff++;
                    else if(tlist[i][0] == 'ENDIF' && !iff) {
                        closed = true;
                        break;
                    } else if(tlist[i][0] == 'ENDIF') iff--;
                    else if(tlist[i][0] == 'ELSE' && !iff && !elsse) elsse = i;
                    else if(tlist[i][0] == 'ELSE' && !iff) throw new Error('Malformed IF/ELSE.');
                }
                if(!closed) {
                    throw new Error('No end tag for IF.');
                    return false;
                }
                if(elsse === false) {
                    var doblock = tlist.slice(0, i);
                    with(ctxt) { var r = eval(c[2]); }
                    var done = r ? this.evaluate(doblock, ctxt) : "";
                } else {
                    var ifblock = tlist.slice(0, elsse);
                    var elseblock = tlist.slice(elsse+1, i);
                    with(ctxt) { var r = eval(c[2]); }
                    var doblock = r ? ifblock : elseblock;
                    var done = this.evaluate(doblock, ctxt);
                }
                return done+this.evaluate(tlist.slice(i));
            }
        },
        'ELSE': {
            pattern: /^else$/,
            'do_': function(c, tlist, ctxt) {
                return this.evaluate(tlist);    
            }

        },
        'ENDIF': {
            pattern: /^endif$/,
            'do_': function(c, tlist, ctxt) {
                return this.evaluate(tlist);    
            }
        },
        'EQ': {
            pattern: /^=\s*([A-Za-z][\]\[A-Za-z0-9._]*)$/,
            'do_': function(c, tlist, ctxt) {
                return pathresolve(c[2], ctxt)+this.evaluate(tlist, ctxt);    
            }
        }
    },
    _debug: function(m) {
        if(this._config.debug) console.log(m);
    },
    _tpush: function(cmd, line, offset) {
        var valid = false;
        for(var t in this._tags) {
            if(cmd.match(this._tags[t].pattern)) {
                valid = true;
                break;
            }
        }
        if(!valid) throw new Error("Invalid tag: "+cmd);
        var th_ = this._tags[t].pattern.exec(cmd);
        var th = [];
        for(var i=0; i<th_.length; i++) th.push(th_[i]);
        th.shift()
        th.unshift({line: line, offset: offset});
        th.unshift(t);
        this._theap.push(th);
        return this;
    },
    trim: function(s) {
        return s.replace(/^\s+|\s+$/g, '');
    },
    chop: function(s) {
        return s.substr(0, s.length-1);
    },
    last_in: function() {
        return this._buffer.length ? this._buffer[this._buffer.length-1] : null;
    },
    scan: function() {
        this._buffer = '';
        this._theap = [];
        var line = 1;
        var offset = 0;
        var src = this._src;
        for(var i=0; i<src.length; i++, offset++) {
            var ch = this._src.charAt(i);
            if(ch == this.D && this.last_in() == this.L) {
                this._theap.push(this.chop(this._buffer)); // put terminals directly on the heap
                this._buffer = '';
            } else if(ch == this.R && this.last_in() == this.D) {
                try {
                    this._tpush(this.trim(this.chop(this._buffer)), line, i); // put non-terminals on the heap as tags
                } catch(e) {
                    throw new Error("Template scan failed at line "+line+":"+i+": "+e);
                }
                this._buffer = '';
            } else {
                this._debug('CAPTURE');
                if(ch == '\n') line++;
                offset = 0;
                this._buffer += ch;
            }
            this._debug('BUFFER: '+this._buffer);
        }
        if(this._buffer.length) {
            this._theap.push(this._buffer);
            this._buffer = '';
        }
    },
    evaluate: function(tlist_, ctxt) {
        if(!tlist_.length) return "";
        var tlist = tlist_.slice();
        var c = tlist.shift();
        var ctxt = ctxt || {};
        return this._tags[c[0]] && this._tags[c[0]].do_ ? this._tags[c[0]].do_.call(this, c, tlist, ctxt) : c+this.evaluate(tlist, ctxt);
    },
    render: function() {
        if(!this._theap || !this._theap.length) this.scan();
        return this.evaluate(this._theap, this._ctxt);
    }
}
