HTMLWidgets.widget({
    name: 'RDataFlow',
    type: 'output',

    factory: function (el, width, height) {
        // TODO: define shared variables for this instance

        return {
            renderValue: function (x) {
                const svg = d3.select("svg");

                svg.selectAll("*").remove();

                const defs = svg.append("defs");

                d3.select(".tooltip").remove();

                const tooltip = d3.select("body").append("div")
                    .attr("class", "tooltip")
                    .style("opacity", 0);

                svg.attrs({
                    "width": width,
                    "height": height
                });

                const variables = x.variables,
                      functions = x.functions,
                      hypotheses = x.hypotheses;

                const color = d3.scaleOrdinal(d3.schemeCategory10);

                color.domain(d3.map(hypotheses, function(d) { return d.name; }).keys());

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
                        if ((arg instanceof Object) && arg.id[0] === "f") {
                            return arg.arguments.reduce(argument_recursion, acc);
                        }

                        acc.push(arg);

                        return acc;
                    }

                    const args = func.arguments.reduce(argument_recursion, []);

                    const products = variables.filter(function (v) { return v.origin === func.id; });

                    func.model_name = args.concat(products)
                        .filter(function (arg) { return (arg instanceof Object) && arg.id[0] === "v" && arg.type === "model"; })
                        .map(function (arg) { return arg.name })
                        .reduce(function (acc, arg) { return acc && acc !== arg ? "…" : arg; }, null);

                    const vars = args
                        .filter(function (arg) { return (arg instanceof Object) && arg.id[0] === "v"; })
                        .concat(products)
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
                    }, []);

                streams.forEach(function (stream, i, arr) {
                    const g = svg.append("g")
                        .attr("transform", "translate(" + i / arr.length * width + "," + 50 + ")");

                    g.append("text")
                        .attrs({
                            x: width / arr.length / 2,
                            y: -20,
                            dy: ".35em"
                        })
                        .style("text-anchor", "middle")
                        .text(stream.name);

                    const tree = d3.tree().size([width / arr.length, height - 80]);

                    stream = d3.stratify()
                        .id(function (d) { return d.id; })
                        .parentId(function (d) { return d.parent; })
                        (stream.functions);

                    /*stream.each(function(d) {
                        d.name = d.data.name;
                    });*/

                    var nodes = d3.hierarchy(stream);

                    // maps the node data to the tree layout
                    nodes = tree(nodes);

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
                                (func.model_name ? "model_node" : "")
                            ].join(" ");
                        })
                        .attr("transform", function (d) {
                            return "translate(" + d.x + "," + d.y + ")";
                        });

                    // adds the circle to the node
                    node.append("circle")
                        .attr("r", 5)
                        .style("fill", function (d) {
                            const func = d.data.data;

                            if (func.categories.length)
                                return color(func.categories[0].name);
                        })
                        .style("stroke", function (d) {
                            const func = d.data.data;

                            if (!func.breakpoint /*&& !d.data.data.model_name*/ && func.categories.length)
                                return color(func.categories[0].name);
                        });

                    node.append("text")
                        .attrs({
                            dy: ".35em",
                            x: 10
                        })
                        .style("text-anchor", "start")
                        .on("mouseover", function(d) {
                            const func = d.data.data;

                            const arguments = func.arguments.reduce(function (acc, e) {
                                if (e instanceof Object) {
                                    var name = e.name;

                                    // if (name.length > 20)
                                    //     name = name.substr(0, 20) + "&hellip;";

                                    acc.push(name);

                                } else if (/*e instanceof String*/ (typeof e) === "string") {
                                    acc.push("\"" + e + "\"")

                                } else {
                                    acc.push(e);
                                }

                                return acc;
                            }, []);

                            tooltip
                                .style("opacity", 0.9)
                                .html(func.name + "(" + arguments.join(",<br />" + Array(func.name.length + 2).join(" ")) + ")")
                                .style("left", (d3.event.pageX) + "px")
                                .style("top", (d3.event.pageY - 28) + "px");
                        })
                        .on("mouseout", function(d) {
                            tooltip.style("opactiy", 0);
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
                            return func.model_name;
                        });
                });

                const legend = svg.append("g")
                    .selectAll(".legend")
                    .data(color.domain())
                    .enter().append("g")
                    .attr("class", "legend")
                    .attr("transform", function (d, i) { return "translate(0," + i * 20 + ")"; });

                // draw legend colored rectangles
                legend.append("rect")
                    .attr("x",      width - 18)
                    .attr("width",  18)
                    .attr("height", 18)
                    .style("fill",  color);

                // draw legend text
                legend.append("text")
                    .attr("x",      width - 24)
                    .attr("y",      9)
                    .attr("dy",     ".35em")
                    .style("text-anchor", "end")
                    .text(function (d) { return d; });
            },

            resize: function (width, height) {
                // TODO: code to re-render the widget with a new size
            }
        };
    }
});