const headings = ['h1', 'h2', 'h3', 'h4', 'h5', 'h6']

function transform(tag) {
  if (headings.includes(tag)) {
    return {
      tag: {
        name: "Heading",
        path: "import Heading from '@/components/Heading';"
      },
      add_props: {
        level: parseInt(tag[1])
      }
    }
  } else if (tag === 'p') {
    return { 
      tag: {
        name: "Paragraph",
        path: "import Paragraph from '@/components/Paragraph';"
      },
      add_props: {}
    }
  } else if (tag === "pre") {
    return {
      tag: {
        name: "PreCodeBlock",
        path: "import PreCodeBlock from '@/components/PreCodeBlock';"
      },
      add_props: {}
    }
  } else if (tag === "math") {
    return {
      tag: {
        name: "Math",
        path: "import Math from '@/components/Math';"
      },
      add_props: {}
    }
  } else if (tag === "div") {
    return {
      tag: {
        name: "Div",
        path: "import Div from '@/components/Div';"
      },
      add_props: {}
    }
  } else {
    return {
      tag: tag,
    }
  }
}

const pandaConfig = {
  transform
}

export default pandaConfig;