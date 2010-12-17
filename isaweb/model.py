import web

db = web.database(dbn='sqlite', db='modules.db')

def search_for_species(field, key, limit=None, offset=None, count=False):

    if '*' in key:
        key=key.replace("*", "%")
        if field=="any":
            key = (web.safestr(key),) * 3
            where = ("id LIKE '%s' OR name LIKE '%s' " +
                     "OR sciname LIKE '%s'") % key
        else:
            where = "%s LIKE '%s'" % (web.safestr(field), web.safestr(key))
    else:
        if field=="any":
            key = (web.safestr(key),) * 3
            where = "id='%s' OR name='%s' OR sciname='%s'" % key
        else:
            where = "%s='%s'" % (web.safestr(field), web.safestr(key))
    
    if count: 
        what="count(*) as count"
        limit=None
        offset=None
    else:
        what="*"

    res=db.select('species', what=what, where=where, 
                  limit=limit, offset=offset)
    return [r for r in res]

def get_modules_for_species(spec):
    def get_mod(s):
        res=db.select('module', what="id", where="species LIKE '%%%s%%'" % s)
        return ";".join([str(r.id) for r in res])
    return [get_mod(s) for s in spec]

def get_modules_for_tissues(tiss):
    def get_mod(s):
        res=db.select('module', what="id", where="tissues LIKE '%%%s%%'" % s)
        return ";".join([str(r.id) for r in res])
    return [get_mod(s) for s in tiss]

def search_for_tissue(field, key, limit=None, offset=None, count=False):

    if '*' in key:
        key=key.replace("*", "%")
        if field=="any":
            key = (web.safestr(key),) * 2
            where = ("id LIKE '%s' OR name LIKE '%s' ") % key
        else:
            where = "%s LIKE '%s'" % (web.safestr(field), web.safestr(key))
    else:
        if field=="any":
            key = (web.safestr(key),) * 3
            where = "id='%s' OR name='%s' " % key
        else:
            where = "%s='%s'" % (web.safestr(field), web.safestr(key))
    
    if count: 
        what="count(*) as count"
        limit=None
        offset=None
    else:
        what="*"

    res=db.select('tissue', what=what, where=where, 
                  limit=limit, offset=offset)
    return [r for r in res]
    

def search_for_gene(field, key, limit=None, offset=None, count=False):

    if '*' in key:
        key=key.replace("*", "%")
        if field=="any":
            key = (web.safestr(key),) * 4
            where = ("ensembl LIKE '%s' OR entrez LIKE '%s' " +
                     "OR symbol LIKE '%s' OR name LIKE '%s'") % key
        else:
            where = "%s LIKE '%s'" % (web.safestr(field), web.safestr(key))
    else:
        if field=="any":
            key = (web.safestr(key),) * 4
            where = "ensembl='%s' OR entrez='%s' OR symbol='%s' OR name='%s'" % key
        else:
            where = "%s='%s'" % (web.safestr(field), web.safestr(key))
    
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
    return [r for r in res]

def search_for_enrichment(field, key, limit=None, offset=None, count=False):

    if '*' in key:
        key=key.replace("*", "%")
        if field=="any":
            key = (web.safestr(key),) * 2
            where = "id LIKE '%s' OR name LIKE '%s'" % key
        else:
            where = "type='%s' AND (id LIKE '%s' OR name LIKE '%s')" % \
                (web.safestr(field), web.safestr(key), web.safestr(key))
    else:
        if field=="any":
            key = (web.safestr(key),) * 2
            where = "id='%s' OR name='%s'" % key
        else:
            where = "type='%s' AND (id='%s' OR name='%s')" % \
                (web.safestr(field), web.safestr(key), web.safestr(key))

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
    return [r for r in res]

def get_module_types():

    return db.select('module_type')

def get_modules(mods):
    res=db.select('module', where='id in (' + 
                  ",".join([str(m) for m in mods]) + ')')
    return [r for r in res]

def get_module(m):
    return db.select('module', where="id=%s" % int(m))

def get_enrichment(c):
    return db.select('enrichment', where="category='%s'" % c)
