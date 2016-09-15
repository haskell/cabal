# -*- coding: utf-8 -*-
import re

from docutils import nodes
from docutils.parsers.rst import Directive, directives, roles

import pygments.lexer as lexer
import pygments.token as token

from distutils.version import StrictVersion

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import ObjType, Domain, Index
from sphinx.domains.std import StandardDomain
from sphinx.locale import l_, _
from sphinx.roles import XRefRole
from sphinx.util.docfields import Field, DocFieldTransformer
from sphinx.util.nodes import make_refnode

def parse_deprecated(txt):
    if txt is None:
        return True
    try:
        return StrictVersion(txt)
    except ValueError:
        return True

def parse_flag(env, sig, signode):
    import re
    names = []
    for i, flag in enumerate(sig.split(',')):
        flag = flag.strip()
        sep = '='
        parts = flag.split('=')
        if len(parts) == 1:
            sep=' '
            parts = flag.split()
        if len(parts) == 0: continue

        name = parts[0]
        names.append(name)
        sig = sep + ' '.join(parts[1:])
        sig = re.sub(ur'<([-a-zA-Z ]+)>', ur'⟨\1⟩', sig)
        if i > 0:
            signode += addnodes.desc_name(', ', ', ')
        signode += addnodes.desc_name(name, name)
        if len(sig) > 0:
            signode += addnodes.desc_addname(sig, sig)

    return names[0]


class Meta(object):
    '''
    Meta data associated with object
    '''
    def __init__(self, since=None, deprecated=None, synopsis=None, title=None, index=0):
        self.since = since
        self.deprecated = deprecated
        self.synopsis = synopsis
        self.title = title
        self.index = index


def find_section_title(parent):
    '''
    Find current section title if possible
    '''
    while parent is not None:
        if isinstance(parent, nodes.section):
            break
        parent = parent.parent
    if parent is None:
        return None

    for kid in parent:
        if isinstance(kid, nodes.title):
            return kid.astext()
    return None


class CabalSection(Directive):
    """
    Marks section to which following objects belong.
    Does not generate any output besides anchor
    """
    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {
        'name': lambda x: x,
        'deprecated': parse_deprecated,
        'since' : StrictVersion,
        'synopsis' : lambda x:x,
        'index' : int
    }
    section_key = 'cabal:pkg-section'
    target_prefix = 'pkg-section-'
    indextemplate = ''
    indextype = 'pair'

    def get_index_entry(self, name):
        return self.indextemplate % name

    def run(self):
        env = self.state.document.settings.env
        section = self.arguments[0].strip()

        if ':' in self.name:
            self.domain, self.objtype = self.name.split(':', 1)
        else:
            self.domain, self.objtype = '', self.name

        if section == 'None':
            env.ref_context.pop(self.section_key, None)
            return []

        env.ref_context[self.section_key] = section
        targetname = self.target_prefix + section
        node = nodes.target('', '', ids=[targetname])
        self.state.document.note_explicit_target(node)

        indexentry = self.get_index_entry(section)

        inode = addnodes.index(
            entries = [
                (self.indextype, indexentry, targetname, '', None)])

        # find title of parent section node
        title = find_section_title(self.state.parent)

        data_key = CabalDomain.types[self.objtype]
        index_key = data_key + '-num'

        # find how many sections in this document were added
        num = env.domaindata['cabal'][index_key].get(env.docname, 0)
        env.domaindata['cabal'][index_key][env.docname] = num + 1

        meta = Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'),
                    synopsis=self.options.get('synopsis'),
                    index = num,
                    title = title)

        store = env.domaindata['cabal'][data_key]
        if not section in store:
            store[section] = env.docname, targetname, meta

        return [inode, node]


class CabalPackageSection(CabalSection):
    """
    Marks section in package.cabal file
    """
    indextemplate = '%s; package.cabal section'
    section_key = 'cabal:pkg-section'
    target_prefix = 'pkg-section-'


class CabalField(ObjectDescription):
    option_spec = {
        'noindex': directives.flag,
        'deprecated': parse_deprecated,
        'since' : StrictVersion,
        'synopsis' : lambda x:x
    }

    doc_field_types = [
        Field('default', label='Default value', names=['default'], has_arg=False)
    ]

    section_key = 'cabal:pkg-section'
    indextemplate = '%s; package.cabal section'

    def get_meta(self):
        # find title of current section, will group references page by it
        section = None
        if isinstance(self.state.parent, nodes.section):
            for kid in self.state.parent:
                if isinstance(kid, nodes.title):
                    section = kid.astext()
        return Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'),
                    synopsis=self.options.get('synopsis'))

    def get_index_entry(self, env, name):
        return name, self.objtype + '-' + name

    def get_env_key(self, env, name):
        section = self.env.ref_context.get(self.section_key)
        store = CabalDomain.types[self.objtype]
        return (section, name), store

    def get_index_entry(self, env, name):
        section = self.env.ref_context.get(self.section_key)

        if section is not None:
            parts = (self.objtype, section, name)
            indexentry = self.indextemplate % (section + ':' + name)
        else:
            parts = (self.objtype, name)
            indexentry = self.indextemplate % name

        targetname = '-'.join(parts)
        return indexentry, targetname


    def handle_signature(self, sig, signode):
        sig = sig.strip()
        parts = sig.split(':',1)
        name = parts[0]
        signode += addnodes.desc_name(name, name)
        signode += addnodes.desc_name(': ', ': ')

        if len(parts) > 1:
            rest = parts[1].strip()
            signode += addnodes.desc_addname(rest, rest)

        meta = self.get_meta()

        rendered = render_meta_title(meta)
        if rendered != '':
            signode += addnodes.desc_addname(' ', ' ')
            signode += addnodes.desc_annotation(rendered, rendered)

        return name

    def add_target_and_index(self, name, sig, signode):
        env = self.state.document.settings.env

        indexentry, targetname = self.get_index_entry(self, name)

        signode['ids'].append(targetname)
        self.state.document.note_explicit_target(signode)

        inode = addnodes.index(
            entries=[('pair', indexentry, targetname, '', None)])
        signode.insert(0, inode)

        meta = self.get_meta()
         #for ref finding

        key, store = self.get_env_key(env, name)
        env.domaindata['cabal'][store][key] = env.docname, targetname, meta

class CabalPackageField(CabalField):
    section_key = 'cabal:pkg-section'
    indextemplate = '%s; package.cabal field'

class CabalFieldXRef(XRefRole):
    section_key = 'cabal:pkg-section'
    def process_link(self, env, refnode, has_explicit_title, title, target):
        parts = target.split(':',1)
        if len(parts) == 2:
            section, target = parts
            section = section.strip()
            target = target.strip()
            refnode[self.section_key] = section
        else:
            refnode[self.section_key] = env.ref_context.get(self.section_key)

        return title, target

class CabalPackageFieldXRef(CabalFieldXRef):
    section_key = 'cabal:pkg-section'

class CabalConfigSection(CabalSection):
    """
    Marks section in package.cabal file
    """
    indextemplate = '%s; project.cabal section'
    section_key = 'cabal:cfg-section'
    target_prefix = 'cfg-section-'

class ConfigField(CabalField):
    section_key = 'cabal:cfg-section'
    indextemplate = '%s ; cabal project option'
    def handle_signature(self, sig, signode):
        sig = sig.strip()
        if sig.startswith('-'):
            name = parse_flag(self, sig, signode)
        else:
            name = super(ConfigField, self).handle_signature(sig, signode)

        return name

    def get_index_entry(self, env, name):
        if name.startswith('-'):
            section = self.env.ref_context.get(self.section_key)
            if section is not None:
                parts = ('cfg-flag', section, name)
                indexname = section + ':' + name
            else:
                parts = ('cfg-flag', name)
                indexname = name
            indexentry = name + '; cabal project option'
            targetname = '-'.join(parts)
            return indexentry, targetname
        else:
            return super(ConfigField,self).get_index_entry(env, name)

    def get_env_key(self, env, name):
        section = self.env.ref_context.get(self.section_key)
        if name.startswith('-'):
            return (section, name), 'cfg-flags'
        return (section, name), 'cfg-fields'

class CabalConfigFieldXRef(CabalFieldXRef):
    section_key = 'cabal:cfg-section'


class ConfigFieldIndex(Index):
    name = 'projectindex'
    localname = "Cabal reference"
    shortname = "Reference"

    def generate(self, docnames=None):
        # (title, section store, fields store)
        entries = [('project.cabal fields', 'cfg-sections', 'cfg-fields'),
                   ('cabal project flags', 'cfg-sections', 'cfg-flags'),
                   ('project.cabal fields', 'pkg-sections', 'pkg-fields')]

        result = []
        for label, section_key, key in entries:

            # sort sections by (index, name)
            sections = sorted(self.domain.data[section_key].items(),
                              key=lambda x: (x[1][2].index, x[0]))
            data = {}
            for (section, name), value in self.domain.data[key].items():
                try:
                    data[section].append((name,value))
                except KeyError:
                    data[section] = [(name,value)]

            references = []
            for section, (sec_doc, sec_anchor, sec_meta) in sections:
                fields = data.get(section, [])
                sec_extra = render_meta(sec_meta)
                sec_descr = sec_meta.synopsis if sec_meta.synopsis is not None \
                            else sec_meta.title if sec_meta.title \
                            else section
                references.append(
                    (sec_descr, 0, sec_doc, sec_anchor, sec_extra, '', ''))
                fields.sort(key = lambda x: x[0])
                for name, (doc, anchor, meta) in fields:
                    extra = render_meta(meta)
                    descr = meta.synopsis if meta.synopsis is not None else ''
                    field = (name, 2, doc, anchor, extra, '', descr)
                    references.append(field)
            result.append((label, references))

        return result, False

def make_data_keys(typ, target, node):
    if typ == 'pkg-field':
        section = node.get('cabal:pkg-section')
        return [(section, target),
                (None, target),
                ('global', target),
                ('build', target)]
    elif typ in ('cfg-field', 'cfg-flag'):
        section = node.get('cabal:cfg-section')
        return [(section, target), (None, target)]
    else:
        return [target]

def render_meta(meta):
    if meta.deprecated is not None:
        if isinstance(meta.deprecated, StrictVersion):
            return 'deprecated since: '+str(meta.deprecated)
        else:
            return 'deprecated'
    elif meta.since is not None:
        return 'since version: ' + str(meta.since)
    else:
        return ''

def render_meta_title(meta):
    rendered = render_meta(meta)
    if rendered != '':
        return '(' + rendered + ')'
    return ''

def make_title(typ, key, meta):
    if typ == 'pkg-section':
        return "package.cabal " + key + " section " + render_meta_title(meta)

    elif typ == 'pkg-field':
        section, name = key
        if section is not None:
            base = "package.cabal " + section + " section " + name + ": field"
        else:
            base = "package.cabal " + name + " field"

        return base + render_meta_title(meta)

    elif typ == 'cfg-section':
        return "project.cabal " + key + " section " + render_meta_title(meta)

    elif typ == 'cfg-field':
        section, name = key
        return "project.cabal " + name + " field " + render_meta_title(meta)

    elif typ == 'cfg-flag':
        section, name = key
        return "cabal flag " + name + " " + render_meta_title(meta)

    else:
        raise ValueError("Unknown type: " + typ)

def make_full_name(typ, key, meta):
    if typ == 'pkg-section':
        return 'pkg-section-' + key

    elif typ == 'pkg-field':
        section, name = key
        if section is not None:
            return '-'.join(('pkg-field',section, name))
        else:
            return 'pkg-field-' + name

    elif typ == 'cfg-field':
        return 'cfg-field-' + key

    else:
        raise ValueError('Unknown object type: ' + typ)

class CabalDomain(Domain):
    name = 'cabal'
    label = 'Cabal'
    object_types = {
        'pkg-section': ObjType(l_('pkg-section'), 'pkg-section'),
        'pkg-field'  : ObjType(l_('pkg-field')  , 'pkg-field'  ),
        'cfg-section': ObjType(l_('cfg-section'), 'cfg-section'),
        'cfg-field'  : ObjType(l_('cfg-field') , 'cfg-field' ),
    }
    directives = {
        'pkg-section': CabalPackageSection,
        'pkg-field'  : CabalPackageField,
        'cfg-section': CabalConfigSection,
        'cfg-field'  : ConfigField,
    }
    roles = {
        'pkg-section': XRefRole(warn_dangling=True),
        'pkg-field'  : CabalPackageFieldXRef(warn_dangling=True),
        'cfg-section': XRefRole(warn_dangling=True),
        'cfg-field'  : CabalConfigFieldXRef(warn_dangling=True),
        'cfg-flag'   : CabalConfigFieldXRef(warn_dangling=True),
    }
    initial_data = {
        'pkg-sections': {},
        'pkg-fields'  : {},
        'cfg-sections': {},
        'pkg-sections-num' : {}, #per document number of sections
        'cfg-sections-num' : {}, #used to order references page
        'cfg-fields'  : {},
        'cfg-flags'   : {},
    }
    indices = [
        ConfigFieldIndex
    ]
    types = {
        'pkg-section': 'pkg-sections',
        'pkg-field'  : 'pkg-fields',
        'cfg-section': 'cfg-sections',
        'cfg-field'  : 'cfg-fields',
        'cfg-flag'   : 'cfg-flags',
    }
    def clear_doc(self, docname):
        for k in ['pkg-sections', 'pkg-fields', 'cfg-sections',
                  'cfg-fields', 'cfg-flags']:
            for name, (fn, _, _) in self.data[k].items():
                if fn == docname:
                    del self.data[k][comname]
        for k in ['pkg-sections-num', 'cfg-sections-num']:
            if docname in self.data[k]:
                self.data[k][docname]

    def resolve_xref(self, env, fromdocname, builder, type, target, node, contnode):
        objtypes = self.objtypes_for_role(type)
        for typ, key in ((typ, key) for typ in objtypes
                                    for key in make_data_keys(typ, target, node)):
            try:
                data = env.domaindata['cabal'][self.types[typ]][key]
            except KeyError:
                continue
            doc, ref, meta = data
            title = make_title(typ, key, meta)
            return make_refnode(builder, fromdocname, doc, ref, contnode, title)

    def get_objects(self):
        for typ in ['pkg-section', 'pkg-field',
                    'cfg-section', 'cfg-field', 'cfg-flag']:
            key = self.types[typ]
            for name, (fn, target, meta) in self.data[key].items():
                title = make_title(typ, name, meta)
                yield title, title, typ, fn, target, 0

class CabalLexer(lexer.RegexLexer):
    name = 'Cabal'
    aliases = ['cabal']
    filenames = ['.cabal']
    flags = re.MULTILINE

    tokens = {
      'root' : [
          (r'^(\s*)(--.*)$', lexer.bygroups(token.Whitespace, token.Comment.Single)),
          # key: value
          (r'^(\s*)([\w\-_]+)(:)',
           lexer.bygroups(token.Whitespace, token.Keyword, token.Punctuation)),
          (r'^([\w\-_]+)', token.Keyword), # library, executable, flag etc.
          (r'[^\S\n]+', token.Text),
          (r'&&|\|\||==|<=|\^>=|>=|<|>', token.Operator),
          (r',|:|{|}', token.Punctuation),
          (r'.', token.Text)
      ],
    }

def setup(app):
    app.add_domain(CabalDomain)
    app.add_lexer('cabal', CabalLexer())

