import web
import re

def unique(seq):
    keys = {}
    for e in seq:
        keys[e] = 1
    return keys.keys()

def unique2(seqx):
    modlist=[ r.split(";") for r in seq if r != '']
    mods=[ i for s in modlist for i in s ] # flatten
    return unique(mods)    

db = web.database(dbn='sqlite', db='modules.db')

def regexp(expr, item):
    return int(re.search(expr, item, re.IGNORECASE) is not None)

def install_regexp():
    db._getctx().db.create_function('REGEXP', 2, regexp)

def where_clause(fields, key, words=True):
    if '%' in key:
        key = (web.websafe(key),) * len(fields)
        clause = "%s LIKE '%s'"
        where = " OR ".join([ clause %(f,k) for f,k in zip(fields, key)])
    else:
        if words:
            install_regexp()
            clause="%s REGEXP '\\b%s\\b'"
        else:
            clause="%s = '%s'"
        key = (web.websafe(key),) * len(fields)
        where=" OR ".join([ clause % (f,k) for f, k in zip(fields, key)])
        
    return "(" + where + ")"

def multi_search(field, fields, keys, combine, words):
    keys=keys.replace("*", "%").split()
    if field=='any':
        field=fields
    where=combine.join(where_clause(field, key, words=words) for key in keys)
    return where

def search_for_species(field, keys, limit=None, offset=None, 
                       words=True, count=False, combine=" OR "):

    where=multi_search(field, ('id', 'name', 'sciname'), keys,
                       combine, words=words)
    
    if count: 
        what="count(*) as count"
        limit=None
        offset=None
    else:
        what="*"

    res=db.select('species', what=what, where=where, 
                  limit=limit, offset=offset)
    return list(res)

def get_modules_for_species(spec, which='3'):

    if which=='1':
        where="id NOT LIKE 'p%' AND "
    elif which=='2':
        where="id LIKE 'p%' AND "
    else:
        where=""

    def get_mod(s):
        res=db.select('module', what="id", 
                      where=where + ("species LIKE '%%%s%%'" % s))
        return ";".join([str(r.id) for r in res])
    return [get_mod(s) for s in spec]

def get_modules_for_tissues(tiss, which='3'):

    if which=='1':
        where="id NOT LIKE 'p%%' AND tissues LIKE '%%%s%%'"
    elif which=='2':
        where="id LIKE 'p%%' AND tissues LIKE '%%%s%%'"
    else:
        where="tissues LIKE '%%%s%%'"

    def get_mod(s):
        res=db.select('module', what="id", where=where % s)
        return ";".join([str(r.id) for r in res])
    return [get_mod(s) for s in tiss]

def search_for_tissue(field, keys, limit=None, offset=None, 
                      words=True, count=False, combine=" OR "):

    where=multi_search(field, ('id', 'name'), keys, combine, words=words)

    if count: 
        what="count(*) as count"
        limit=None
        offset=None
    else:
        what="*"

    res=db.select('tissue', what=what, where=where, 
                  limit=limit, offset=offset)
    return list(res)
    
def search_for_gene(field, keys, limit=None, offset=None, words=True, 
                    count=False, combine=" OR "):

    where=multi_search(field, ('ensembl', 'entrez', 'symbol', 'name'), keys,
                       combine, words=words)

    if count: 
        what="count(*) as count"
        limit=None
        offset=None
        order=None
    else:
        what="*"
        order="symbol"

    res=db.select('gene', what=what, where=where, 
                  limit=limit, offset=offset, order=order)
    return list(res)

def search_for_enrichment(field, keys, limit=None, offset=None, 
                          words=True, count=False, combine=" OR "):

    where=multi_search(field, ('id', 'name'), keys, combine, words=words)

    if count: 
        what="count(*) as count"
        limit=None
        offset=None
        order=None
    else:
        what="*"
        order="name"

    res=db.select('category', what=what, where=where, 
                  limit=limit, offset=offset, order=order)
    return list(res)

def get_module_types():

    return db.select('module_type')

def get_modules(mods):
    res=db.select('module', where='id in (' + 
                  ",".join(["'%s'" % str(m) for m in mods]) + ')')
    return list(res)

def get_module(m):
    return db.select('module', where="id='%s'" % m)

def get_enrichment(c):
    return db.select('enrichment', where="category='%s'" % c)

def modules_for_species(field, keys, mods, which, words=True, combine=" OR "):
    where=multi_search(field, ('id', 'name', 'sciname'), keys, combine,
                       words=words)
    spec=[s.id for s in db.select('species', what='id', where=where)]
    spec=unique(spec)
    if len(spec)==0:
        return mods
    where=" OR ".join("species LIKE '%%%s%%'" % s for s in spec)
    if which=='1':
        where="id NOT LIKE 'p%' AND (" + where + ")"
    elif which=='2':
        where="id LIKE 'p%' AND (" + where + ")"
    specmods=db.select('module', what='id,species', where=where)
    for rec in specmods:
        m=str(rec.id)
        for s in rec.species.split(";"):
            s2=s.split(":")[1]
            if s2 in spec:
                if m in mods:
                    mods[m]=mods[m] + '; <span class="s">%s</span>' % s2
                else:
                    mods[m]='<span class="s">%s</span>' % s2
    return mods

def modules_for_tissue(field, keys, mods, which, words=True, combine=" OR "):
    where=multi_search(field, ('id', 'name'), keys, combine, words=words)
    tiss=[t.id for t in db.select('tissue', what='id', where=where)]
    tiss=unique(tiss)
    if len(tiss)==0:
        return mods
    where=" OR ".join("tissues LIKE '%%%s%%'" % t for t in tiss)
    if which=='1':
        where="id NOT LIKE 'p%' AND (" + where + ")"
    elif which=='2':
        where="id LIKE 'p%' AND (" + where + ")"
    tissmods=db.select('module', what='id,tissues', where=where)
    for rec in tissmods:
        m=str(rec.id)
        for t in rec.tissues.split(";"):
            t2=t.split(":")[1]
            if t2 in tiss:
                if m in mods:
                    mods[m]=mods[m] + '; <span class="t">%s</span>' % t2
                else:
                    mods[m]='<span class="t">%s</span>' % t2
    return mods

def modules_for_gene(field, keys, mods, which, words=True, combine=" OR "):
    where=multi_search(field, ("ensembl", "entrez", "symbol", "name"), 
                       keys, combine, words=words)
    genes=db.select("gene", what='modules,symbol', where=where)
    for rec in genes:
        if rec.modules=='':
            continue;
        for m in rec.modules.split(";"):
            if (which=='1' and m[0]=='p') or (which=='2' and m[0]!='p'):
                continue
            s=rec.symbol.replace(";", ", ")
            if m in mods:
                mods[m]=mods[m] + '; <span class="g">%s</span>' % s
            else:
                mods[m]='<span class="g">%s</span>' % s
    return mods

def modules_for_enrichment(field, keys, mods, which, words=True,
                           combine=" OR "):
    where=multi_search(field, ('id', 'name'), keys, combine, words=words)
    cats=db.select('category', what="modules,name", where=where)
    for rec in cats:
        if rec.modules=='':
            continue;
        for m in rec.modules.split(";"):
            if (which=='1' and m[0]=='p') or (which=='2' and m[0]!='p'):
                continue
            n=rec.name
            if m in mods:
                mods[m]=mods[m] + '; <span class="e">%s</span>' % n
            else:
                mods[m]='<span class="e">%s</span>' % n
    return mods

