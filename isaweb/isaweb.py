#! /usr/bin/python

import web
import model
import re
import odict

### URL mappings

urls = (
    '/?',                                            'Index',
    '/search/?',                                     'Search',
    '/search/(\w+)/(\w+)/([ \w+\-\*]+)/(\d+)/(\d+)', 'Search',
    )

app=web.application(urls, globals())

def set_home(handle):
    globals['home'] = web.ctx.home;
    return handle()

app.add_processor(set_home)

def unique(seq):
    keys = {}
    for e in seq:
        keys[e] = 1
    return keys.keys()

### Templates

globals = { 'sub': re.sub,
            'home': '',
            'static': '/cbg/ISA/species/static',
            'isahtmlpath': 'http://www2.unil.ch/cbg/ISA/species/isa8-html2/',
            'isahtmlpath2': 'http://www2.unil.ch/cbg/ISA/species/isa10-html/', 
            'ensemblurl' : 'http://www.ensembl.org/%s/geneview?gene=%s',
            'entrezurl': 'http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?db=gene&cmd=Retrieve&dopt=full_report&list_uids=%s',
            'gourl': 'http://amigo.geneontology.org/cgi-bin/amigo/go.cgi?view=details&search_constraint=terms&depth=0&query=%s',
            'keggurl': 'http://www.genome.ad.jp/dbget-bin/show_pathway?hsa%s',
            'genecardsurl': 'http://www.genecards.org/cgi-bin/carddisp.pl?gene=%s',
            'omimurl': 'http://www.ncbi.nlm.nih.gov/omim/%s',
            'atlasurl': 'http://human.brain-map.org/search.html?search_type=microarray&query=%s'}

render = web.template.render('templates', base='base', globals=globals)
render_plain = web.template.render('templates', globals=globals)

class Index:
    
    def GET(self):
        contents=model.get_module_types()
        return render.index(contents)

class Search:

    def get_modules(self, modstrings=''):
        modlist=[ r.split(";") for r in modstrings if r != '']
        mods=[ i for s in modlist for i in s ] # flatten
        mods=model.get_modules(unique(mods))
        modules={}
        for m in mods:
            modules[str(m.id)]=m
        return modules

    def search_for_gene(self, field, key, limit, offset):
        res=model.search_for_gene(field, key, limit, offset)
        modules=self.get_modules(modstrings=[ r.modules for r in res ])
        return render_plain.search_gene(res, modules)

    def get_pvals(self, res, modules):
        def getp(rec):
            p={}
            for r in rec:
                p[r.module] = "%.3e" % r.pvalue
            return p
        enr={}
        for r in res:
            enr[r.id] = getp(model.get_enrichment(r.id))
        return enr

    def search_for_enrichment(self, field, key, limit, offset):
        res=model.search_for_enrichment(field, key, limit, offset)
        modules=self.get_modules(modstrings=[ r.modules for r in res ])
        pvals=self.get_pvals(res, modules)
        return render_plain.search_enrichment(res, modules, pvals)

    def search_for_species(self, field, key, limit, offset):
        res=model.search_for_species(field, key, limit, offset)
        spec=[s.id for s in res]
        mods=model.get_modules_for_species(spec)
        for n in range(len(res)):
            res[n].modules=mods[n]
        modules=self.get_modules(modstrings=mods)
        return render_plain.search_species(res, modules)

    def search_for_tissue(self, field, key, limit, offset):
        res=model.search_for_tissue(field, key, limit, offset)
        tiss=[t.id for t in res]
        mods=model.get_modules_for_tissues(tiss)
        for n in range(len(res)):
            res[n].modules=mods[n]
        modules=self.get_modules(modstrings=mods)
        return render_plain.search_tissue(res, modules)

    def get_pos(self, hitcounts, offset, limit):
        offsets=odict.odict()
        limits=odict.odict()

        cs=0
        for c, o in hitcounts.items():
            if offset < cs + o:
                offsets[c] = offset-cs
                limits[c] = min(limit, o-offsets[c])
                offset = offset + limits[c]
                limit = limit-limits[c]
            else:
                offsets[c] = 0
                limits[c] = 0
            cs = cs + o

        return (offsets, limits)

    def get_search_expression(self, what, field, key, limit, offset):
        return key

    def GET(self, what="any", field="any", key="*", limit=10, offset=0):
        if what not in ("any", "gene", "enrichment", "species", "tissue"):
            return web.notfound()

        sexp=self.get_search_expression(what, field, key, limit, offset)

        limit=int(limit)
        offset=int(offset)

        html=odict.odict()        
        html['species']=html['tissue']=html['gene']=html['enrichment']=''
        hitcounts=odict.odict()
        hitcounts['species']=hitcounts['tissue']=hitcounts['gene']= \
            hitcounts['enrichment']=0
        chitcounts=odict.odict()

        ## Get the hit counts
        hitcounts['species']= \
            model.search_for_species(field, key, count=True)[0].count
        chitcounts['species']=0
        hitcounts['tissue']=\
            model.search_for_tissue(field, key, count=True)[0].count
        chitcounts['tissue']=hitcounts['species']
        hitcounts['gene']= \
            model.search_for_gene(field, key, count=True)[0].count
        chitcounts['gene']=chitcounts['tissue']+hitcounts['tissue'];
        hitcounts['enrichment']= \
            model.search_for_enrichment(field, key, count=True)[0].count
        chitcounts['enrichment']=chitcounts['gene']+hitcounts['gene']

        ## What to show
        offsets, limits = self.get_pos(hitcounts, offset, limit)

        ## Show it
        if what in ('species', 'any') and limits['species'] != 0:
            html['species']=self.search_for_species(field, key, 
                                                    limits['species'],
                                                    offsets['species'])
        if what in ('tissue', 'any') and limits['tissue'] != 0:
            html['tissue']=self.search_for_tissue(field, key, 
                                                  limits['tissue'],
                                                  offsets['species'])
        if what in ('gene', 'any') and limits['gene'] != 0:
            html['gene']=self.search_for_gene(field, key, limits['gene'], 
                                              offsets['gene'])
        if what in ('enrichment', 'any') and limits['enrichment'] != 0:
            html['enrichment']= \
                self.search_for_enrichment(field, key, limits['enrichment'], 
                                           offsets['enrichment'])

        return render.searchresults(hitcounts, chitcounts, 
                                    html, offset, web.ctx.home,
                                    field, key, limit, what,
                                    sum(hitcounts.values()), 
                                    sexp)

    def POST(self):
        inp=web.input()
        web.seeother("/search/any/any/*" + inp.key.strip("*") + "*/10/0")
        
app.run()

