const path = require("path");

const HtmlWebpackPlugin = require("html-webpack-plugin");
const merge = require("webpack-merge");

const pretty = it => JSON.stringify(it, null, "  ");

const devConfig = (env, argv, { mode, rootDir }) => {
    if (mode !== "development") {
        return {};
    }

    return {
        devServer: {
            compress: true,
            contentBase: path.resolve(rootDir, "./dist"),
            port: 8081,
        },
        mode,
    };
};

const prodConfig = (env, argv, { mode }) => {
    if (mode !== "production") {
        return {};
    }

    return {
        mode,
    };
};

const baseConfig = (env, argv, { rootDir }) => ({
    entry: [
        path.resolve(rootDir, "./js/index.js"),
    ],
    module: {
        rules: [{
            exclude: [/elm-stuff/, /node_modules/],
            test: /\.elm$/,
            use: {
                loader: "elm-webpack-loader",
                options: {
                    cwd: path.resolve(rootDir, "./"),
                },
            },
        }],
    },
    output: {
        filename: "app.js?[chunkhash:12]",
        path: path.resolve(rootDir, "./dist"),
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: path.resolve(rootDir, "./index.template.html"),
        }),
    ],
});

module.exports = (env, argv) => {
    const settings = {
        mode: env.NODE_ENV || "development",
        rootDir: __dirname,
    };

    console.log("webpack.settings", pretty(settings));

    const use = fragment => fragment(env, argv, settings);

    const config = merge.smart(
        use(baseConfig),
        use(devConfig),
        use(prodConfig)
    );

    console.log("webpack.config", pretty(config));

    return config;
};
