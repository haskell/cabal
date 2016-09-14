import re

from docutils import nodes
from docutils.parsers.rst import Directive, directives, roles

import pygments.lexer as lexer
import pygments.token as token

from distutils.version import StrictVersion

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import ObjType, Domain
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


class CabalPackageSection(Directive):
    """
    Directive marks a package.cabal section described next
    and adds it to index. Can be referenced with pkg-directive.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {
        'deprecated': parse_deprecated,
        'since' : StrictVersion
    }

    def run(self):
        env = self.state.document.settings.env
        section = self.arguments[0].strip()


        if section == 'None':
            env.ref_context.pop('cabal:section', None)
            return []

        env.ref_context['cabal:section'] = section
        targetname = 'pkg-section-' + section
        node = nodes.target('', '', ids=[targetname])
        self.state.document.note_explicit_target(node)
        indexentry = section + '; package.cabal section'
        inode = addnodes.index(entries=[('pair', indexentry,
                                         targetname, '', None)])

        meta = Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'))

        env.domaindata['cabal']['pkg-sections'][section] = \
            env.docname, targetname, meta

        return [inode, node]

class Meta(object):
    def __init__(self, since=None, deprecated=None):
        self.since = since
        self.deprecated = deprecated

class CabalField(ObjectDescription):
    option_spec = {
        'noindex': directives.flag,
        'deprecated': parse_deprecated,
        'since' : StrictVersion
    }

    doc_field_types = [
        Field('default', label='Default value', names=['default'], has_arg=False)
    ]

    def get_meta(self):
        return Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'))

    def get_index_entry(self, env, name):
        return name, self.objtype + '-' + name

    def get_env_key(self, env, name):
        return self.name

    def handle_signature(self, sig, signode):
        sig = sig.strip()
        parts = sig.split(':',1)
        name = parts[0]
        signode += addnodes.desc_name(name, name)
        signode += addnodes.desc_name(':', ':')

        if len(parts) > 1:
            rest = parts[1].strip()
            signode += addnodes.desc_addname(rest, rest)

        meta = self.get_meta()

        rendered = render_meta(meta)
        if rendered != '':
            signode += addnodes.desc_annotation(' ' + rendered, ' '+rendered)

        return name

    def add_target_and_index(self, name, sig, signode):
        env = self.state.document.settings.env

        indexentry, targetname = self.get_index_entry(self, name)

        signode['ids'].append(targetname)
        self.state.document.note_explicit_target(signode)

        inode = addnodes.index(
            entries=[('pair', indexentry, targetname, '', None)])
        signode.insert(0, inode)

        meta = Meta(since=self.options.get('since'),
                    deprecated=self.options.get('deprecated'))
         #for ref finding
        key = self.get_env_key(env, name)
        store = CabalDomain.types[self.objtype]
        env.domaindata['cabal'][store][key] = env.docname, targetname, meta

class CabalPackageField(CabalField):
    def get_index_entry(self, env, name):
        section = self.env.ref_context.get('cabal:section')

        if section is not None:
            parts = (self.objtype, section, name)
            indexentry = section + ':' + name + '; package.cabal field'
        else:
            parts = (self.objtype, name)
            indexentry = name + '; package.cabal field'

        targetname = '-'.join(parts)
        return indexentry, targetname

    def get_env_key(self, env, name):
        section = env.ref_context.get('cabal:section')
        return section, name

class CabalPackageFieldXRef(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        parts = target.split(':',1)
        if len(parts) == 2:
            section, target = parts
            section = section.strip()
            target = target.strip()
            refnode['cabal:section'] = section
        else:
            refnode['cabal:section'] = env.ref_context.get('cabal:section')
        return title, target


def make_data_keys(typ, target, node):
    if typ == 'pkg-field':
        section = node.get('cabal:section')
        return [(section, target), (None, target)]
    else:
        return [target]

def render_meta(meta):
    if meta.deprecated is not None:
        if isinstance(meta.deprecated, StrictVersion):
            return ' (deprecated since:'+str(meta.deprecated) + ')'
        else:
            return '(deprecated)'
    elif meta.since is not None:
        return ' (since version: ' + str(meta.since) + ')'
    else:
        return ''

def make_title(typ, key, meta):
    if typ == 'pkg-section':
        return "package.cabal " + key + "section" + render_meta(meta)

    if typ == 'pkg-field':
        section, name = key
        if section is not None:
            base = "package.cabal " + section + " section " + name + " field"
        else:
            base = "package.cabal " + name + " field"

        return base + render_meta(meta)

def make_full_name(typ, key, meta):
    if typ == 'pkg-section':
        return 'pkg-section-' + key

    if typ == 'pkg-field':
        section, name = key
        if section is not None:
            return '-'.join(('pkg-field',section, name))
        else:
            return 'pkg-field-' + name

class CabalDomain(Domain):
    name = 'cabal'
    label = 'Cabal'
    object_types = {
        'pkg-section': ObjType(l_('pkg-section'), 'pkg-section'),
        'pkg-field'  : ObjType(l_('pkg-field')  , 'pkg-field'  ),
    }
    directives = {
        'pkg-section': CabalPackageSection,
        'pkg-field'  : CabalPackageField
    }
    roles = {
        'pkg-section': XRefRole(warn_dangling=True),
        'pkg-field'  : CabalPackageFieldXRef(warn_dangling=True),
    }
    initial_data = {
        'pkg-sections': {},
        'pkg-fields': {},
    }
    indices = [

    ]
    types = {
        'pkg-section': 'pkg-sections',
        'pkg-field'  : 'pkg-fields'
    }
    def clear_doc(self, docname):
        for k in ['pkg-sections', 'pkg-fields']:
            for name, (fn, _, _) in self.data[k].items():
                if fn == docname:
                    del self.data[k][comname]

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
        for typ in ['pkg-section', 'pkg-field']:
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

