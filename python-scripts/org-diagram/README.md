# Org to Graphviz DOT Converter

This script converts an Org-mode outline into a Graphviz DOT source block and generates a visual representation of the outline as an image. The script supports various statuses and priorities to visually differentiate tasks.

## Features

- **High Priority (C)** tasks are represented with bold and all caps text.
- **Medium Priority (B)** tasks are represented with bold text.
- **Low Priority (A)** tasks are represented with regular text.
- **TODO** tasks are colored light green.
- **IN PROGRESS** tasks are colored deep sky blue.
- **BLOCKED** tasks are colored orange.
- **DONE** tasks are shown with a strikethrough.

## Requirements

- Python 3
- Graphviz

## Installation

1. Install Graphviz:
   ```sh
   sudo apt-get install graphviz
   ```

2. Ensure you have Python 3 installed.

## Usage

1. Save the script as `org_to_dot.py`.
2. Run the script with an Org-mode file as an argument:
   ```sh
   python3 org_to_dot.py <path-to-org-file>
   ```

The script will generate an image (`output.png`) representing the Org-mode outline.

## Example

Given the following Org-mode outline in `sops.org`:

```org
* Generate SOPS Configs :project:
** DONE [#A] run sops config pipeline 
*** Generate .enc. Files 
***** get repository deployments 
****** [#B] get deployment variabl Default colores from each deployment
******* create sops env.enc file for us-west-2 
******** generates for dev, uat, prod
****** if account=stage :work:
******* BLOCKED [#C] create sops env.enc file with 526083761887
****** if deploy=prod: create sops env.enc file us-east-1 
*** TODO Generate sops.yaml
*** IN PROGRESS hello :StartedMentioned:
```

Running the script:

```sh
python3 org_to_dot.py sops.org
```

Produces an image `output.png` with the visual representation of the outline.
