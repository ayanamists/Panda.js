import re
import os

def transform_md_to_html(input_str):
    pattern = r'{{<\s*admonition "(.*?)"\s*>}}(.*?){{<\s*/admonition\s*>}}'
    replacement = r'<div class="admonition" data-admonition-type="\1">\2</div>'

    output_str = re.sub(pattern, replacement, input_str, flags=re.DOTALL)

    return output_str

def std_test():
    md_str = '''

    # aaa

    {{<  admonition "warning" >}}
    This article was translated from Chinese by ChatGPT and may contain some errors.
    {{< /admonition >}}

    bbb
    '''

    html_str = transform_md_to_html(md_str)
    print(html_str)

dir = "src/contents"
markdown_list = os.listdir(dir)
for file in markdown_list:
    if ".md" not in file:
        continue
    abs_path = os.path.join(dir, file)
    with open(abs_path) as f:
        data = f.read()
        new = transform_md_to_html(data)
    with open(abs_path, "w") as f:
        f.write(new)