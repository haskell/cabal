import re

from docutils import nodes
from docutils.parsers.rst import Directive, directives, roles

import pygments.lexer as lexer
import pygments.token as token

from sphinx import addnodes
from sphinx.directives import ObjectDescription
from sphinx.domains import ObjType, Domain
from sphinx.domains.std import StandardDomain
from sphinx.locale import l_, _
from sphinx.roles import XRefRole
from sphinx.util.nodes import make_refnode

class CabalPackageSection(Directive):
    """
    Directive marks a package.cabal section described next
    and adds it to index. Can be referenced with pkg-directive.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}

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

        env.domaindata['cabal']['pkg-sections'][section] = \
            env.docname, targetname

        return [inode, node]

class CabalPackageField(ObjectDescription):
    def handle_signature(self, sig, signode):
        sig.strip()
        parts = sig.split(':',1)
        name = parts[0]
        signode += addnodes.desc_name(name+':', name+':')

        if len(parts) > 1:
            rest = parts[1]
            rest.strip()
            signode += addnodes.desc_annotation(rest, rest)
        return name

    def add_target_and_index(self, name, sig, signode):
        env = self.state.document.settings.env

        section = self.env.ref_context.get('cabal:section')

        if section is not None:
            parts = (self.objtype, section, name)
            indexentry = section + ':' + name + '; package.cabal field'
        else:
            parts = (self.objtype, name)
            indexentry = name + '; package.cabal field'

        targetname = '-'.join(parts)
        signode['ids'].append(targetname)
        self.state.document.note_explicit_target(signode)

        inode = addnodes.index(entries=[('pair', indexentry,
                                         targetname, '', None)])
        signode.insert(0, inode)

        #for ref finding
        env.domaindata['cabal']['pkg-fields'][section,name] = \
            env.docname, targetname

class CabalPackageFieldXRef(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        parts = target.split(':',1)
        if len(parts) == 2:
            section, target = parts
            section.strip()
            target.strip()
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

def make_title(typ, key):
    if typ == 'pkg-section':
        return "package.cabal " + key + "section"

    if typ == 'pkg-field':
        section, name = key
        if section is not None:
            return "package.cabal " + section + " section " + name + " field"
        else:
            return "package.cabal " + name + " field"

def make_full_name(typ, key):
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
        'objects': {},
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
        for k in ['objects', 'pkg-sections', 'pkg-fields']:
            for name, (fn, _) in self.data[k].items():
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
            doc, ref = data
            title = make_title(typ, key)
            return make_refnode(builder, fromdocname, doc, ref, contnode, title)

    def get_objects(self):
        for typ in ['pkg-section', 'pkg-field']:
            key = self.types[typ]
            for name, (fn, target) in self.data[key].items():
                title = make_title(typ, name)
                yield title, title, typ, fn, target, 0

class CabalLexer(lexer.RegexLexer):
    name = 'Cabal'
    aliases = ['cabal']
    filenames = ['.cabal']
    flags = re.MULTILINE

    tokens = {
      'root' : [
          (r'\n', token.Text),
          (r'^\s*(--.*)$', token.Comment.Single),
          # key: value
          (r'^(\s*)([\w\-_]+)(:)',
           lexer.bygroups(token.Whitespace, token.Keyword, token.Punctuation)),
          (r'^([\w\-_]+)', token.Keyword), # library, executable, flag etc.
          (r'[^\S\n]+', token.Text),
          (r'(\n\s*|\t)', token.Whitespace),
          (r'&&|\|\||==|<=|>=|<|>|^>=', token.Operator),
          (r',|:|{|}', token.Punctuation),
          (r'.', token.Text)
      ],
    }

def setup(app):
    app.add_domain(CabalDomain)
    app.add_lexer('cabal', CabalLexer())

