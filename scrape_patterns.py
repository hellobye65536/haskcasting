#!/usr/bin/env python3
import itertools
import re
import sys
import textwrap
from typing import cast

from bs4 import BeautifulSoup

soup = BeautifulSoup(sys.stdin, "html.parser")

title_regex = re.compile(r"(.+?)\s*(\((.*?)\s*→\s*(.*?)\))?")

name_replacements = [
    ("Rfln.", "Reflection"),
    ("Refl.", "Reflection"),
    ("Prfn.", "Purification"),
    ("Purif.", "Purification"),
    ("Dstl.", "Distillation"),
    # ('Tan.', 'Tangent'),
    ('Arch.', 'Archgenie'),
]

known_types = {
    "any": "IotaAny",
    "null": "IotaNull",
    #
    "number": "IotaNumber",
    "num": "IotaNumber",
    #
    "vector": "IotaVector",
    "vec": "IotaVector",
    #
    "pattern": "IotaPattern",
    #
    "entity": "IotaEntity",
    "entity | null": "IotaEntity",
    "item entity": "IotaEntity",
    "player": "IotaEntity",
    "animated scroll entity": "IotaEntity",
    "speck entity": "IotaEntity",
    #
    "bool": "IotaBoolean",
    "boolean": "IotaBoolean",
    #
    "list": "IotaAnyList",
    #
    "identifier": "IotaIdentifier",
}


def parse_types(ts: str | None):
    return [] if ts is None else [s.strip() for s in ts.split(",")]


def make_type(ty: str) -> str:
    r = known_types.get(ty)
    if r:
        return r

    list_prefix = "list of "
    if ty.startswith(list_prefix):
        r = ty[len(list_prefix) :]
        if r[-1] == "s":
            r = r[:-1]
        r = make_type(r)
        if " " in r:
            return f"IotaList ({r})"
        else:
            return f"IotaList {r}"
    return ty


def make_types(tys: list[str]) -> list[list[str]]:
    match tys:
        case []:
            return [[]]
        case [ty, *tys]:
            rs = [make_type(t) for t in ty.split("/")]
            nrs = make_types(tys)
            return [[r, *nr] for r in rs for nr in nrs]
        case _:
            raise RuntimeError("unreachable")


def show_types(tys: list[str]) -> str:
    return f"'[{', '.join(tys)}]"


for section in soup.main("section", recursive=False):  # pyright: ignore[reportOptionalCall]
    section_title = section.h2.get_text().strip()  # pyright: ignore[reportOptionalMemberAccess]
    section_first = True
    for subsection in section("div", recursive=False):
        if subsection.h3 is None:
            continue
        subsection_title = subsection.h3.get_text().strip()
        subsection_first = True

        # for pattern in subsection('h4'):
        for pattern in subsection("h4", class_="pattern-title"):
            pattern_title = pattern.get_text().strip()

            pattern_title_match = title_regex.fullmatch(pattern_title)
            if pattern_title_match is None:
                raise RuntimeError(pattern_title)

            pattern_name = pattern_title_match.group(1)
            for name, repl in name_replacements:
                pattern_name = pattern_name.replace(name, repl)
            if "." in pattern_name:
                raise RuntimeError(pattern_title)

            pattern_inputs = parse_types(pattern_title_match.group(3))
            pattern_outputs = parse_types(pattern_title_match.group(4))
            pattern_inputs.reverse()
            pattern_outputs.reverse()

            data = pattern.parent.canvas  # pyright: ignore[reportOptionalMemberAccess]
            if data is None:
                continue

            ident_name = "".join(c for c in pattern_name if c.isalpha())
            direction = cast(str, data["data-start"]).upper()
            angles = cast(str, data["data-string"])
            great_spell = data["data-per-world"] == "True"

            great_name = (
                f'''
                     "{pattern_name}"'''
                if great_spell
                else ""
            )

            input_types = make_types(pattern_inputs)
            output_types = make_types(pattern_outputs)

            shown_types: list[str] = []

            vars = "abcdefgh"
            for itys, otys in itertools.product(input_types, output_types):
                max_vars = 0
                for i, ty in enumerate(itys):
                    if "IotaAny" in ty and ty != "IotaAnyList":
                        itys[i] = itys[i].replace("IotaAny", vars[max_vars])
                        max_vars += 1
                forall = f"forall {' '.join(vars[:max_vars])}. " if max_vars > 0 else ""
                shown_types.append(
                    f"[t|{forall}{show_types(itys)} -> {show_types(otys)}|]"
                )

            types_list: str
            match shown_types:
                case []:
                    types_list = "[]"
                case [ty]:
                    types_list = f"[{ty}]"
                case tys:
                    sep = """
                     , """
                    types_list = f"""[ {sep.join(tys)}
                     ]"""

            print(
                textwrap.dedent(f"""\
                $( mk{"Great" if great_spell else ""}IotaFragExpr
                     "{ident_name}"{great_name}
                     [pattern| {direction} {angles} |]
                     {types_list}
                     -- {pattern_inputs} -> {pattern_outputs}
                 )
            """)
            )

            # output.writerow((
            #     ''.join(c for c in pattern_name if c.isalpha()),
            #     data['data-start'].upper(),
            #     data['data-string'],
            #     pattern_inputs,
            #     pattern_outputs,
            #     1 if data['data-per-world'] == 'True' else 0,
            # ))
