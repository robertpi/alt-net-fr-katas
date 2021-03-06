﻿using System;
using Microsoft.VisualStudio.TestTools.UnitTesting;

using Numerobis;

namespace NumerobisTest
{
    public class TestsBase
    {
        protected void AssertEquals(int expected, string romain)
        {
            var scribe = new Scribe();
            var res = scribe.XXX(romain);
            Assert.AreEqual(expected, res);
        }
        
    }


    [TestClass]
    public class TestsChiffres: TestsBase
    {
        [TestMethod]
        public void I_Vaut_Un()
        {
            AssertEquals(1, "I");
        }


        [TestMethod]
        public void V_Vaut_Cinq()
        {
            AssertEquals(5, "V");
        }


        [TestMethod]
        public void X_Vaut_Dix()
        {
            AssertEquals(10, "X");
        }


        [TestMethod]
        public void L_Vaut_Cinquante()
        {
            AssertEquals(50, "L");
        }


        [TestMethod]
        public void C_Vaut_Cent()
        {
            AssertEquals(100, "C");
        }


        [TestMethod]
        public void D_Vaut_CinqCents()
        {
            AssertEquals(500, "D");
        }
        [TestMethod]
        public void M_Vaut_Mille()
        {
            AssertEquals(1000, "M");
        }

    }
}
