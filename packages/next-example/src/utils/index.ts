export function joinChildren(children: React.ReactDOM, delimiter: string): string {
  if (typeof children === "string") {
    return children;
  } else if (Array.isArray(children)) {
    return children.map((x) => joinChildren(x, delimiter))
      .reduce((a, b) => a + delimiter + b);
  } else {
    return "";
  }
}

export function formatDate(ds: string) {
  const d = new Date(ds);
  return formatNum(d.getDate()) + "/" +
    formatNum(d.getMonth() + 1) + "/" + d.getFullYear();
}

function formatNum(n: number) {
  return n.toLocaleString('en-US', { minimumIntegerDigits: 2, useGrouping: false })
}
