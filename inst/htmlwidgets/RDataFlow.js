HTMLWidgets.widget({
    name: 'RDataFlow',
    type: 'output',

    factory: function (el, width, height) {
        // TODO: define shared variables for this instance
        const container = d3.select(el);

        const menu = container.select("#menu");
        const svg = container.select("svg");
        const selector = container.select("#selector > ul");
        const legend = container.select("#legend > ul");
        const tooltip = container.select("#tooltip")
            .on("mouseleave", function () {
                tooltip.style("display", "none");
            });

        const tooltip_content = tooltip.select("div");

        return {
            renderValue: function (x) {
                svg.selectAll("*").remove();
                selector.selectAll("*").remove();
                legend.selectAll("*").remove();

                const defs = svg.append("defs");

                tooltip.attr("style", null);
                tooltip_content.selectAll("*").remove();

                const variables = x.variables,
                      functions = x.functions,
                      hypotheses = x.hypotheses;

                const color = d3.scaleOrdinal(d3.schemeCategory10);

                color.domain(d3.map(hypotheses, function(d) { return d.id; }).keys());

                const streams = functions.reduce(function (acc, func, i, arr) {
                    // Scope reduction
                    const arg_functions = functions.filter(function(f, ii) { return ii < i; }).filter(function (f) { return func.arguments.indexOf(f.id) !== -1; }),
                          arg_variables = variables.filter(function (v) { return func.arguments.indexOf(v.id) !== -1; }),
                          arg_hypotheses = hypotheses.filter(function (h) { return func.arguments.indexOf(h.id) !== -1; });

                    // Replacement of the arguments in a list with the appropriate entities
                    func.arguments = func.arguments.map(function (arg) {
                        const f = arg_functions.reduce(function (acc, f) { return f.id === arg ? f : acc; }, null),
                              v = arg_variables.reduce(function (acc, v) { return v.id === arg ? v : acc; }, null),
                              h = arg_hypotheses.reduce(function (acc, h) { return h.id === arg ? h : acc; }, null);

                        if (f !== null)
                            return f;

                        if (v !== null)
                            return v;

                        if (h !== null)
                            return h;

                        return arg;
                    });

                    const own_categories = hypotheses
                              .filter(function (h) { return h.functions.indexOf(func.id) !== -1 ||
                                                            arg_variables.reduce(function(acc, v) { return acc || (h.models.indexOf(v.id) !== -1); }, false); }),
                          child_categories = arg_functions
                              .reduce(function (acc, f) { return acc.concat(f.categories); }, []);

                    func.categories = own_categories.concat(child_categories);

                    // Since the nested function won't be showing up on the top level,
                    //     we can interrupt here and use the accumulated stuff further on
                    //     when the top-level function is invoked
                    if (func.depth !== 1) {
                        return acc;
                    }

                    function argument_recursion (acc, arg, i, arr) {
                        if ((arg instanceof Object) && !(arg instanceof Array) && arg.hasOwnProperty("id") && arg.id[0] === "f") {
                            return arg.arguments.reduce(argument_recursion, acc);
                        }

                        acc.push(arg);

                        return acc;
                    }

                    const products = variables.filter(function (v) { return v.origin === func.id; });

                    const args = func.arguments
                        .reduce(argument_recursion, [])
                        .filter(function (arg) { return (arg instanceof Object) && !(arg instanceof Array) && arg.hasOwnProperty("id") && arg.id[0] === "v"; })
                        .concat(products);

                    const vars = args
                        .reduce(function (acc, arg, i, arr) {
                            while (arg.generation > 0) {
                                if (arg.precursors.length) {
                                    const precursors = variables.filter(function (v) { return arg.precursors.indexOf(v.id) !== -1; });

                                    const data_precursors = precursors.filter(function (v) { return v.type === "data"; });

                                    if (arg.type === "data" && data_precursors.length === 0) {
                                        // Assume that the data variable was constructed from the constants
                                        //   and reset it's 'generation' attribute as if it was a regular data variables
                                        arg.generation = 0;

                                    } else if (arg.type === "data") {
                                        acc.push(arg);

                                        arg = data_precursors[0];

                                    } else {
                                        acc.push(arg);

                                        arg = precursors[0];
                                    }

                                } else if (arg.type === "column") {
                                    acc.push(arg);

                                    var tmp = variables.reduce(function (acc, v) { return v.columns.indexOf(arg.id) !== -1 ? v : acc; }, null);

                                    if (tmp !== null)
                                        arg = tmp;

                                } else {
                                    break;
                                }
                            }

                            acc.push(arg);

                            return acc;
                        }, [])
                        .filter(function (arg) { return arg.type === "data"; })
                        .reduce(function (acc, arg, i, arr) {
                            if (acc.indexOf(arg) === -1)  // indexOf may not work properly, but == should
                                acc.push(arg);

                            return acc;
                        }, []);

                    func.breakpoint = vars.reduce(function (acc, arg) { return acc || func.breakpoint === arg.id }, false);

                    const there_are_models = args.reduce(function(acc, arg, i, arr) { return acc || arg.type === "model"; }, false);

                    func.marker = args
                        .filter(function (arg, i, arr) { return arg.type === "model" || (!there_are_models && arg.type === "data" && arg.generation > 1); })
                        .map(function (arg, i, arr) { return arg.name })
                        .reduce(function (acc, arg, i, arr) { return acc && acc !== arg ? "…" : arg; }, null);

                    // Stream-work
                    vars.filter(function (arg, i, arr) { return arg.generation === 0; })
                        .forEach(function (arg, i, arr) {
                            var stream = acc.reduce(function (acc, stream) { return stream.id === arg.id ? stream : acc; }, null);

                            // Cloning because stream overlaps
                            const notch = JSON.parse(JSON.stringify(func));
                            notch.parent = null;

                            if (stream === null) {
                                stream = {
                                    name: arg.name,
                                    id: arg.id,
                                    functions: []
                                };

                                acc.push(stream);

                            } else {
                                notch.parent = stream.functions
                                    .filter(function(old_notch, i, arr) {
                                        return (i === 0) ||
                                            old_notch.breakpoint ||
                                            (!old_notch.categories.length && !notch.categories.length) ||
                                            (old_notch.categories.length && notch.categories.length &&
                                             old_notch.categories[0].id === notch.categories[0].id);
                                    })
                                    .reverse()
                                    [0].id;
                            }

                            stream.functions.push(notch);
                        });

                        return acc;
                    }, [])
                    .sort(function (a, b) {
                        return b.functions.length - a.functions.length;  // Descending order
                    });

                selector.selectAll("li")
                    .data(streams).enter()
                    .append("li")
                    .text(function (d) { return d.name + ":" + d.functions.length; })
                    .attr("title", function (d) { return d.name; })
                    .on("click", draw);

                function draw (stream_, i, arr) {
                    svg.selectAll(".stream").remove();

                    const g = svg.append("g")
                        .attr("class", "stream");

                    var stream = d3.stratify()
                        .id(function (d) { return d.id; })
                        .parentId(function (d) { return d.parent; })
                        (stream_.functions);

                    var nodes = d3.hierarchy(stream);

                    const depth = nodes.height;

                    function breadth_seek(acc, node) {
                        if (node.hasOwnProperty("children")) {
                            node.children.forEach(function (node) {
                                acc = breadth_seek(acc, node);
                            });
                        }

                        acc[node.depth] = acc[node.depth] ? acc[node.depth] + 1 : 1;

                        return acc;
                    }

                    const breadth = Math.max.apply(null, breadth_seek(Array(depth), nodes));

                    const tree = d3.tree().size([breadth * 120, depth * 30]);

                    // maps the node data to the tree layout
                    nodes = tree(nodes);

                    g.append("text")
                        .attrs({
                            x: nodes.x,
                            y: -20,
                            dy: ".35em"
                        })
                        .style("text-anchor", "middle")
                        .text(stream_.name);

                    var link = g.selectAll(".link")
                        .data(nodes.descendants().slice(1))
                        .enter().append("path")
                        .attrs({
                            class: "link",
                            d: function (d) {
                                return "M" + d.x + "," + d.y
                                    + "C" + d.x + "," + (d.y + d.parent.y) / 2
                                    + " " + d.parent.x + "," + (d.y + d.parent.y) / 2
                                    + " " + d.parent.x + "," + d.parent.y;
                            }
                        });

                    // adds each node as a group
                    var node = g.selectAll(".node")
                        .data(nodes.descendants())
                        .enter().append("g")
                        .attr("class", function (d) {
                            const func = d.data.data;

                            return [
                                "node",
                                (d.children ? "node--internal" : "node--leaf"),
                                (func.breakpoint ? "breakpoint" : ""),
                                (func.marker ? "marker" : "")
                            ].join(" ").trim();
                        })
                        .attr("transform", function (d) {
                            return "translate(" + d.x + "," + d.y + ")";
                        })
                        .on("click", function(d) {
                            const func = d.data.data;
                            Shiny.setInputValue("goto", func.lines);
                        });

                    // adds the circle to the node
                    node.append("circle")
                        .attr("r", 5)
                        .style("fill", function (d) {
                            const func = d.data.data;

                            if (func.categories.length)
                                return color(func.categories[0].id);
                        })
                        .style("stroke", function (d) {
                            const func = d.data.data;

                            if (!func.breakpoint /*&& !func.marker*/ && func.categories.length)
                                return color(func.categories[0].id);
                        });

                    node.append("text")
                        .attrs({
                            dy: ".35em",
                            x: 10
                        })
                        .style("text-anchor", "start")
                        .on("mouseover", function(d) {
                            const func = d.data.data;

                            tooltip
                                .styles({
                                    "display": "block",
                                    "left": (d3.event.pageX) + "px",
                                    "top": (d3.event.pageY - 20) + "px"
                                });

                            tooltip_content.selectAll("*").remove();

                            const comment_re = /# *(.+)$/ig;

                            const comment_start = func.signature.search(comment_re);

                            if (comment_start !== -1) {
                                const comment = comment_re.exec(func.signature)[1];

                                // TODO: reformat the signature
                                tooltip_content.append("p")
                                    .text(func.signature.substr(0, comment_start).trim());

                                tooltip_content.append("p")
                                    .attr("class", "comment")
                                    .html("<span>Tail comment:</span> " + comment);

                            } else {
                                tooltip_content.append("p")
                                    .text(func.signature);
                            }

                            if (!!func.packages) {
                                tooltip_content.append("hr");

                                const help = tooltip_content.append("p")
                                    .text("From packages: ");

                                func.packages.forEach(function (pkg, i, arr) {
                                    help.append("a")
                                        .attrs({
                                            href: pkg,
                                            class: "help"
                                        })
                                        .on("click", function () {
                                            Shiny.setInputValue("help", [pkg, func.name]);
                                        })
                                        .text(pkg);

                                    if (i !== arr.length - 1)
                                        help.append(",")
                                });
                            }
                        })
                        .on("mouseout", function (d) {
                            const x = d3.event.pageX;
                            const y = d3.event.pageY;

                            const bbox = tooltip.node().getBoundingClientRect();

                            // if (!(x >= bbox.left - 10 && x <= bbox.right + 10 && y >= bbox.top - 10 && y <= bbox.bottom + 10)) {
                            if (!(x >= bbox.left && x <= bbox.right && y >= bbox.top && y <= bbox.bottom)) {
                                tooltip.style("display", "none");
                            }
                        })
                        .text(function (d) {
                            const func = d.data.data;
                            return func.name;
                        });

                    node.append("text")
                        .attrs({
                            dy: ".35em",
                            x: -10
                        })
                        .style("text-anchor", "end")
                        .text(function (d) {
                            const func = d.data.data;
                            return func.marker;
                        });

                    const bbox = g.node().getBoundingClientRect();

                    g.attr("transform", "translate(" + -(bbox.left + svg.node().parentNode.scrollLeft - svg.node().offsetLeft - 25) + ", 50)");

                    svg.attrs({
                        width: bbox.width + 50,
                        height: bbox.height + 50
                    });

                    const stream_categories = stream_.functions
                        .reduce(function (acc, func, i, arr) {
                            return acc.concat(func.categories);
                        }, [])
                        .map(function (e) { return e.id })
                        .filter(function (e, i, arr) {
                            return arr.indexOf(e) === i;
                        });

                    legend.selectAll("*").remove();

                    const categories = legend.selectAll("li")
                        .data(hypotheses)
                        .enter().append("li")
                        .attr("class", function (d) {
                            return ["legend",
                                stream_categories.indexOf(d.id) === -1 ? "useless" : ""
                            ].join(" ").trim();
                        })
                        .attr("title", function (d) { return d.name; })
                        .on("click", function (d) {
                            Shiny.setInputValue("edit_hypothesis", d.id);
                        })
                        .text(function (d) { return d.name; });

                    categories.append("span")
                        .style("background-color", function(d) { return color(d.id); });

                    selector.selectAll("li")
                        .attr("class", null)
                        .filter(function (d, ii) { return ii === i;})
                        .attr("class", "chosen");
                }

                if (streams.length) {
                    draw(streams[0], 0, streams);
                }
            },

            resize: function (width, height) {
                // TODO: code to re-render the widget with a new size
            }
        };
    }
});