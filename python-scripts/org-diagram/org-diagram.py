import re
import sys
import tempfile
import subprocess


class TreeNode:
    def __init__(self, label, edge_label=None):
        self.label = label
        self.edge_label = edge_label
        self.children = []

    def add_child(self, child_node):
        self.children.append(child_node)


def parse_outline_headline(headline):
    """Parse the HEADLINE for priority, status, and tags, returning cleaned headline, color, style, and edge label."""
    # Capitalize keywords
    headline = re.sub(
        r"\b(todo|in progress|blocked|done)\b",
        lambda m: m.group(0).upper(),
        headline,
        flags=re.IGNORECASE,
    )

    priority_match = re.search(r"\[#([A-C])\]", headline)
    priority = priority_match.group(1) if priority_match else None

    status_match = re.search(r"^(TODO|IN PROGRESS|BLOCKED|DONE)", headline)
    status = status_match.group(1) if status_match else None

    tags_match = re.search(r":([^:]+):$", headline)
    edge_label = tags_match.group(1) if tags_match else None

    cleaned_headline = re.sub(
        r"\[#([A-C])\]|(TODO|IN PROGRESS|BLOCKED|DONE) |:[^:]+:$", "", headline
    )

    # Determine color and style
    color = {
        "TODO": "lightgreen",
        "IN PROGRESS": "deepskyblue",
        "BLOCKED": "orange",
        "DONE": "lightgrey",
    }.get(status, None)

    style = f"style=filled, fillcolor={color}" if color else ""

    # Modify headline for priorities
    if priority == "B":
        cleaned_headline = f"<b>{cleaned_headline}</b>"
    elif priority == "C":
        cleaned_headline = f"<b>{cleaned_headline.upper()}</b>"

    # Strike through for done status
    if status == "DONE":
        cleaned_headline = f"<s>{cleaned_headline}</s>"

    return cleaned_headline, style, edge_label


def split_string_by_word_limit(string, word_limit):
    """Split STRING into substrings of WORD-LIMIT words."""
    words = string.split()
    return "<br/>".join(
        " ".join(words[i : i + word_limit]) for i in range(0, len(words), word_limit)
    )


def build_tree_from_outline(outline):
    """Build a tree structure from the Org-mode outline."""
    root = TreeNode("root")
    stack = [(root, 0)]

    for line in outline.splitlines():
        match = re.match(r"^(?P<level>\*+)\s(?P<headline>.+)$", line)
        if match:
            level = len(match.group("level"))
            headline = match.group("headline")
            cleaned_headline, style, edge_label = parse_outline_headline(headline)
            node = TreeNode((cleaned_headline, style), edge_label)

            while stack and stack[-1][1] >= level:
                stack.pop()

            stack[-1][0].add_child(node)
            stack.append((node, level))

    return root


def generate_dot_from_tree(
    node, dot_nodes, dot_edges, node_id=0, parent_id=None, word_limit=3
):
    """Generate DOT nodes and edges from the tree structure."""
    current_id = node_id
    if node.label != "root":
        label, style = node.label
        node_label = split_string_by_word_limit(label, word_limit)
        dot_nodes.append(
            f"node_{current_id} [label=<{node_label}>, shape=box, {style}];"
        )
        if parent_id is not None:
            edge_label = f'xlabel="{node.edge_label}"' if node.edge_label else ""
            dot_edges.append(f"node_{parent_id} -> node_{current_id} [{edge_label}];")
        parent_id = current_id
        current_id += 1

    for child in node.children:
        current_id = generate_dot_from_tree(
            child, dot_nodes, dot_edges, current_id, parent_id, word_limit
        )

    return current_id


def format_dot_source_block(dot_nodes, dot_edges):
    """Format DOT-NODES and DOT-EDGES into a DOT source block with a legend."""
    legend = [
        "subgraph cluster_legend {",
        '  label="Legend";',
        "  key [label=<",
        '    <table border="0" cellborder="1" cellspacing="0">',
        '      <tr><td bgcolor="lightgreen">TODO</td><td bgcolor="deepskyblue">In Progress</td></tr>',
        '      <tr><td bgcolor="orange">Blocked</td><td bgcolor="lightgrey"><s>Done</s></td></tr>',
        "    </table>",
        "  >, shape=plaintext];",
        "}",
    ]
    return "\n".join(
        [
            "digraph Org {",
            '  node [fontname="Helvetica, Arial, sans-serif"];',
            "  graph [splines=ortho];",  # Use orthogonal edges for a cleaner layout
            "\n".join(dot_nodes),
            "\n".join(dot_edges),
            "\n".join(legend),
            "}",
        ]
    )


def org_outline_to_dot_src_block(file_path, word_limit=3):
    """Convert the Org-mode outline to a Graphviz DOT source block."""
    with open(file_path, "r") as file:
        content = file.read()

    # Extracting only the headings
    outline = "\n".join(line for line in content.splitlines() if line.startswith("*"))
    tree_root = build_tree_from_outline(outline)

    dot_nodes, dot_edges = [], []
    generate_dot_from_tree(tree_root, dot_nodes, dot_edges, word_limit=word_limit)
    dot_src = format_dot_source_block(dot_nodes, dot_edges)
    return dot_src


def save_dot_and_generate_image(dot_src, output_image="output.png"):
    """Save DOT source to a file and generate an image using Graphviz."""
    with tempfile.NamedTemporaryFile(delete=False, suffix=".dot") as dot_file:
        dot_file.write(dot_src.encode("utf-8"))
        dot_file_path = dot_file.name

    subprocess.run(["dot", "-Tpng", dot_file_path, "-o", output_image])
    print(f"Generated image saved as {output_image}")


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python script.py <org-file-path>")
        sys.exit(1)

    org_file_path = sys.argv[1]

    with open(org_file_path, "r") as file:
        org_content = file.read()

    dot_src = org_outline_to_dot_src_block(org_file_path)
    save_dot_and_generate_image(dot_src)
