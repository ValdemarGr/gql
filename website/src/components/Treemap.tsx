import * as d3 from 'd3';
import React from 'react';

type NodeInfo = {
    text?: JSX.Element | JSX.Element[],
    hide?: boolean,
    widthMul?: number
}

type Node = {
    id: string,
    info?: NodeInfo,
    cost: number,
    children?: string[]
}

type Props = {
    root: string,
    nodes: Node[],
    height?: number
}

function getWidths(n: Node, all: Record<string, Node>): Record<string, number> {
    if (n.children) {
        const children: Record<string, number> = n.children.map(c => getWidths(all[c], all)).reduce((acc, next) => ({ ...acc, ...next }), {})
        const width = n.children.map(c => children[c]).reduce((acc, next) => acc + next, 0)
        return {
            [n.id]: width,
            ...children
        }
    } else {
        return { [n.id]: 1 }
    }
}

function longestPath(n: Node, accum: number, all: Record<string, Node>): number {
    if (n.children) {
        return Math.max(...n.children.map(c => longestPath(all[c], accum + n.cost, all)))
    } else {
        return accum + n.cost
    }
}

function padChildren(xs: Node[], accum: number, widths: Record<string, number>): [Node, number][] {
    if (xs.length === 0) {
        return []
    } else {
        const [hd, ...tl] = xs
        return [[hd, accum], ...padChildren(tl, accum + widths[hd.id], widths)]
    }
}

type ShowProps = {
    n: Node,
    leftPad: number,
    currentCost: number,
    heightPerCost: number,
    all: Record<string, Node>,
    widths: Record<string, number>,
    maxCost: number,
    maxWidth: number,
}

function ShowNode(p: ShowProps) {
    const padded = padChildren((p.n.children || []).map(c => p.all[c]), p.leftPad, p.widths)

    const mkRect = () => {
        if (p.n.info && !(p.n.info.hide ?? false)) {
            const width = ((p.widths[p.n.id] / p.maxWidth) * 100 * (p.n.info.widthMul || 1)).toString() + "%"
            const height = p.n.cost * p.heightPerCost
            const x = ((p.leftPad / p.maxWidth) * 100)
            const y = p.currentCost * p.heightPerCost


            const fill = "rgb(" + ((p.currentCost / p.maxCost) * 128 / 2 + 32).toString() + ", 32, 32)"
            const style = {
                'strokeWidth': 6,
                stroke: "rgb(128,128,128)"
            }

            return <rect
                width={width}
                height={height}
                x={x.toString() + "%"}
                y={y}
                // random color
                fill={fill}
                style={style}
            />
        } else {
            return <></>
        }
    }

    const height = p.n.cost * p.heightPerCost
    const x = ((p.leftPad / p.maxWidth) * 100)
    const y = p.currentCost * p.heightPerCost

    const txts = p.n.info ? (p.n.info.text instanceof Array ? p.n.info.text : [p.n.info.text]): []

    const txtX = (x + 1).toString() + "%"

    const txt = <text
        x={txtX}
        y={y + height / 2}
        style={{
            'fontSize': 20,
            'fill': 'white',
            'textAnchor': 'start',
            'alignmentBaseline': 'middle'
        }}
    >
        {txts.map((e, i) => {
            const dy = i === 0 ? (("-" + (txts.length / 2 - 0.7) * 1.5).toString() + "em") : "1.5em"
            return <tspan x={txtX} dy={dy}>{e}</tspan>
        })}
    </text>;

    return (
        <>
            <g>
                {mkRect()}
                {txt}
            </g>
            {padded.map(c => {
                const [c0, leftPad] = c
                return <ShowNode
                    n={c0}
                    leftPad={leftPad}
                    currentCost={p.currentCost + p.n.cost}
                    heightPerCost={p.heightPerCost}
                    all={p.all}
                    widths={p.widths}
                    maxCost={p.maxCost}
                    maxWidth={p.maxWidth}
                />
            })}
        </>
    );
}

type State = {
    lookup: Record<string, Node>,
    root: Node,
    widths: Record<string, number>,
    lp: number,
}

export default function Treemap(p: Props) {
    const { lookup, root, widths, lp } = React.useMemo<State>(() => {
        const lookup: Record<string, Node> = p.nodes.reduce((acc, next) => ({ ...acc, [next.id]: next }), {})
        const root = lookup[p.root]
        return {
            lookup: lookup,
            root: root,
            widths: getWidths(root, lookup),
            lp: longestPath(root, 0, lookup)
        }
    }, [])

    const h = p.height || 500;

    const heightPerCost = h / lp;

    return (
        <svg width="100%" height={(h + 20).toString() + "px"}>
            <ShowNode
                n={root}
                leftPad={0}
                currentCost={0}
                heightPerCost={heightPerCost}
                all={lookup}
                widths={widths}
                maxCost={lp}
                maxWidth={(Math.max(...Object.values(widths)))}
            />
        </svg>
    )
}