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
                    const nested_functions = functions.filter(function (n_func) { return func.arguments.indexOf(n_func.id) !== -1; });

                    func.arguments = func.arguments.map(function (arg) {
                        const ff = nested_functions.filter(function (func) { return func.id === arg; });

                        if (ff.length)
                            return ff[0];

                        return arg;
                    });

                    if (func.depth !== 1) {
                        return acc;
                    }

                    function rec (acc, arg, i, arr) {
                        if (arg instanceof Object) {
                            return arg.arguments.reduce(rec, acc);
                        }

                        acc.push(arg);

                        return acc;
                    }

                    const args = func.arguments.reduce(rec, []);

                    const vars = variables.filter(function (variable) { return args.indexOf(variable.id) !== -1; }),
                          products = variables.filter(function (variable) { return variable.origin === func.id; }),
                          hyps = hypotheses.filter(function (hypothesis) { return args.indexOf(hypothesis.id) !== -1; });

                    vars.concat(products).forEach(function (arg, i, arr) {
                        const categories = hypotheses.filter(function (hypothesis) {
                            return hypothesis.functions.indexOf(func.id) !== -1 ||
                                hypothesis.models.indexOf(arg.id) !== -1;
                        });

                        func.category = categories.length ? categories[0].name : null;

                        while (arg.precursors.length || arg.type === "column") {
                            if (arg.precursors.length) {
                                arg = variables.filter(function (variable) {
                                    return variable.id === arg.precursors[0];
                                })[0]

                            } else if (arg.type === "column") {
                                arg = variables.filter(function (variable) {
                                    return variable.columns.indexOf(arg.id) !== -1;
                                })[0]
                            }
                        }

                        // If this is a column -- skip
                        if (variables.filter(function (variable) { return variable.columns.indexOf(arg.id) !== -1; }).length)
                            return;

                        func.breakpoint = func.breakpoint && func.breakpoint === arg.id;

                        var stream = acc.filter(function (stream) { return stream.name === arg.name; });

                        if (!stream.length) {
                            stream = {
                                name: arg.name,
                                // id: arg.id,
                                functions: []
                            };

                            acc.push(stream);
                            func.parent = null;

                        } else {
                            stream = stream[0];

                            if (stream.functions.filter(function(ffunc) { return ffunc.id === func.id; }).length)
                                return;

                            func.parent = stream.functions
                                .filter(function(notch, i, arr) {
                                    return (i === 0) ||
                                        notch.breakpoint ||
                                        (notch.category === func.category);
                                })
                                .reverse()
                                [0].id;
                        }

                        stream.functions.push(func);
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
                            return [
                                "node",
                                (d.children ? "node--internal" : "node--leaf"),
                                d.data.data.breakpoint? "breakpoint" : ""
                            ].join(" ");
                        })
                        .attr("transform", function (d) {
                            return "translate(" + d.x + "," + d.y + ")";
                        });

                    // adds the circle to the node
                    node.append("circle")
                        .attr("r", 5)
                        .style("fill", function (d) {
                            if (d.data.data.category)
                                return color(d.data.data.category);
                        })
                        .style("stroke", function (d) {
                            if (!d.data.data.breakpoint && d.data.data.category)
                                return color(d.data.data.category);
                        });

                    node.append("text")
                        .attrs({
                            dy: ".35em",
                            x: 10
                        })
                        .style("text-anchor", "left")
                        .text(function (d) {
                            return d.data.data.name;
                        });

                    var legend = svg.append("g")
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
                });
            },

            resize: function (width, height) {
                // TODO: code to re-render the widget with a new size
            }
        };
    }
});