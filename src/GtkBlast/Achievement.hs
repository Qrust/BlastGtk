module GtkBlast.Achievement
    (achievements
    ,getAchievement
    ,getAchievementString) where
import Import

achievements :: [(Int, Text)]
achievements =
        reverse $
        [(10, "Анон")
        ,(20, "Няша")
        ,(50, "Троллер")
        ,(100, "Сотня разорванных анусов")
        ,(150, "Вайпер")
        ,(200, "Братишка")
        ,(300, "Бэтмен")
        ,(500, "Ультрахардкорщик")
        ,(1000, "Тысяча порванных срак")
        ,(3000, "Поехавший")
        ,(20000, "Поле устиланное вырванными анусами")
        ,(50000, "Принцесса в другом замке")
        ,(100000, "Обосрался блядь")
        ,(200000, "Проебал все геты")
        ,(1000000, "Chitak ebaniy")
        ]

getAchievement :: Int -> Maybe Text
getAchievement a =
    findMap (\(p, t) -> if a >= p then Just t else Nothing) $ achievements

getAchievementString :: Int -> Text
getAchievementString =
    maybe "" (("Achievement unlocked: \"" ++) . (++ "\"")) . getAchievement
