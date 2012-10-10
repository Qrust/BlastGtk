module GtkBlast.Achievement
    (achievements
    ,getAchievement
    ,getAchievementString) where
import Import

achievements :: [(Int, String)]
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
        ,(20000, "Поле устиланное вырванными аналами неверных")
        ,(50000, "Прости Марио, но Принцесса в другом замке")
        ,(100000, "Супер-пупер-мега-гипер-охуительный пиздец")
        ,(200000, "Словил все геты разом")
        ,(1000000, "Накрутил же, бака")
        ]

getAchievement :: Int -> Maybe String
getAchievement a =
    findMap (\(p, t) -> if a >= p then Just t else Nothing) $ achievements

getAchievementString :: Int -> String
getAchievementString =
    maybe [] (("\nAchievement unlocked: \"" ++) . (++ "\"")) . getAchievement
