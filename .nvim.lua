require("overseer").register_template({
    name = "Build",
    condition = {
        dir = vim.fn.getcwd(),
    },
    builder = function()
        return {
            cmd = { "stack" },
            args = { "build" },
        }
    end,
})
require("overseer").register_template({
    name = "Clean",
    condition = {
        dir = vim.fn.getcwd(),
    },
    builder = function()
        return {
            cmd = { "stack" },
            args = { "clean" },
        }
    end,
})
require("overseer").register_template({
    name = "Test",
    condition = {
        dir = vim.fn.getcwd(),
    },
    builder = function()
        return {
            cmd = { "stack" },
            args = { "test" },
        }
    end,
})
