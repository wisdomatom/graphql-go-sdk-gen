package generator

import (
	"gopkg.in/natefinch/lumberjack.v2"
	"io"
	"os"
	"path/filepath"

	"github.com/sirupsen/logrus"
)

// 定义全局的 Logger 实例，方便在其他文件中引用
var Log = logrus.New()

func init() {
	// 1. 设置日志格式为 TextFormatter (默认且适用于本地查看)
	// 如果需要机器解析 (例如上传到 ELK Stack)，请使用 logrus.JSONFormatter{}
	Log.SetFormatter(&logrus.TextFormatter{
		FullTimestamp:   true,
		TimestampFormat: "2006-01-02 15:04:05.000", // 统一时间格式
		DisableColors:   false,                     // 写入文件时通常禁用颜色
		DisableQuote:    true,                      // <== 不对字符串加引号
	})

	// 设置日志级别：确保所有信息（Debug, Info, Warn, Error, Fatal）都能被记录
	// 在生产环境中，可以设置为 logrus.InfoLevel 或 logrus.WarnLevel
	Log.SetLevel(logrus.DebugLevel)

	// 2. 配置日志切割 (使用 lumberjack)
	logDir := "./logs"
	logFileName := "log"

	// 确保日志目录存在
	if _, err := os.Stat(logDir); os.IsNotExist(err) {
		os.Mkdir(logDir, 0755)
	}

	lumberjackLogger := &lumberjack.Logger{
		Filename:   filepath.Join(logDir, logFileName), // 日志文件的完整路径
		MaxSize:    10,                                 // 日志文件达到 10MB 时开始切割 (MB)
		MaxBackups: 5,                                  // 最多保留 5 个旧的日志文件
		MaxAge:     30,                                 // 保留旧日志文件的最大天数
		Compress:   true,                               // 启用旧日志文件压缩 (gzip)
	}

	// 3. 将 Logrus 的输出目标设置为 lumberjackLogger
	//log.SetOutput(lumberjackLogger)

	// 如果您希望同时输出到控制台 (Stdout) 和文件，可以结合使用 io.MultiWriter
	mw := io.MultiWriter(os.Stdout, lumberjackLogger)
	Log.SetOutput(mw)

	// 可选：将标准库的log也重定向到logrus
	// log.StandardLogger().Hooks.Add(&logrus_syslog.SyslogHook{...})
}
