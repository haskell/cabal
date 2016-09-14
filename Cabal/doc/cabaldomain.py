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

class ExtraData(object):
    def __init__(self, title=None, parent=None):
        self.title = title
        self.parent = parent

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

        extra = ExtraData(title = section)
        env.domaindata['cabal']['pkg-sections'][section] = \
            env.docname, targetname, extra

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
            indexentry = name + '; package.config ' + section + ' field '
        else:
            parts = (self.objtype, name)
            indexentry = name + '; package.config field'

        targetname = '-'.join(parts)
        signode['ids'].append(targetname)
        self.state.document.note_explicit_target(signode)

        inode = addnodes.index(entries=[('pair', indexentry,
                                         targetname, '', None)])
        signode.insert(0, inode)

        #for ref finding
        extra = ExtraData(title = section, parent=section)
        env.domaindata['cabal']['pkg-fields'][section,name] = \
            env.docname, targetname, extra

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

def make_title(typ, name, extras):
    title = extras.title if extras.title else name
    if typ == 'pkg-section':
        return "package.cabal " + title + "section"

    if typ == 'pkg-field':
        if extras.parent is not None:
            base = "package.cabal " + extras.parent + "section field "
        else:
            base = "package.cabal field "
        return base + title


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
        for fullname, (fn, _) in self.data['objects'].items():
            if fn == docname:
                del self.data['objects'][fullname]
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
            doc, name, extras = data
            title = make_title(typ, name, extras)
            return make_refnode(builder, fromdocname, doc, name, contnode, title)

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

